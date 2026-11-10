//! Code used by `with_state_node!`.
//!
//! This macro creates a Godot node for one kind of exported entity data. The
//! node stores the current and previous values. It also lets Godot ask, with a
//! normal typed call, whether an entity has this feature.

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    Ident, Path, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

struct Spec {
    config: Path,
    state: Ident,
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut config: Option<Path> = None;
        let mut state: Option<Ident> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "config" => config = Some(input.parse()?),
                "state" => state = Some(input.parse()?),
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("unknown key `{other}`; expected `config` or `state`"),
                    ));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        Ok(Self {
            config: config.ok_or_else(|| {
                syn::Error::new(Span::call_site(), "missing `config: TransferConfig`")
            })?,
            state: state.ok_or_else(|| {
                syn::Error::new(Span::call_site(), "missing `state: SomeStateNode`")
            })?,
        })
    }
}

pub fn expand(input: TokenStream) -> TokenStream {
    let Spec { config, state } = parse_macro_input!(input as Spec);
    let config_trait = format_ident!("{state}TransferConfig");
    let export_config_trait = format_ident!("{state}ExportConfig");
    let export_state_trait = format_ident!("{state}ExportState");
    let dto = quote! { <#config as #config_trait>::DtoType };
    let change_source_ident = format_ident!("{state}ChangeSource");
    let prepared_ident = format_ident!("{state}PreparedValue");
    let value_trait_ident = format_ident!("{state}ExportValue");
    let meta_ident = format_ident!("{state}EntityMeta");
    let subscription_ident = format_ident!("{state}ValueSubscription");
    let snapshots_ident = format_ident!("{state}Snapshots");
    let pair_ident = format_ident!("{state}SnapshotPair");
    let snapshot = quote! { <#config as #config_trait>::Snapshot };
    let snapshot_imports = quote! {
        use bevy_godot4::prelude::{SnapshotConfig as #snapshots_ident, SnapshotPair as #pair_ident};
    };
    let snapshot_field = quote! { snapshots: <#snapshot as #snapshots_ident<#dto>>::Store, };
    let snapshot_init = quote! { snapshots: Default::default(), };
    let snapshot_publish = quote! {
        <#snapshot as #snapshots_ident<#dto>>::publish(&mut self.snapshots, entity_id, curr);
    };
    let snapshot_remove = quote! {
        <#snapshot as #snapshots_ident<#dto>>::remove(&mut self.snapshots, entity_id);
    };
    let snapshot_read = quote! {
        /// Read the config's published values; DTO-only configs reuse their existing objects.
        pub fn native_values_for(&self, entity_id: i64) -> Option<#pair_ident<<#snapshot as #snapshots_ident<#dto>>::Value>> {
            <#snapshot as #snapshots_ident<#dto>>::read(&self.snapshots, entity_id, || {
                Some(#pair_ident { current: self.curr_for(entity_id)?, previous: self.prev_for(entity_id)? })
            })
        }
    };

    let expanded = quote! {
        use godot::prelude::*;
        use bevy_godot4::prelude::{DataTransferConfig as #config_trait, ExportConfig as #export_config_trait, ExportState as #export_state_trait};
        use bevy_godot4::prelude::ExportChangeSource as #change_source_ident;
        use bevy_godot4::prelude::ExportValue as #value_trait_ident;
        use bevy_godot4::prelude::{EntityMeta as #meta_ident, ExportValueSubscription as #subscription_ident};
        #snapshot_imports

        #[derive(Clone)]
        struct #prepared_ident(Option<Gd<#dto>>);

        impl #value_trait_ident for #prepared_ident {
            const NAME: &'static str = stringify!(#state);
            fn write_arguments(self, arguments: &mut Array<Variant>) {
                arguments.clear();
                arguments.push(&self.0.to_variant());
            }
        }

        #[derive(GodotClass)]
        #[class(base=Node)]
        pub struct #state {
            #snapshot_field
            change_source: Gd<#change_source_ident>,
            entries: std::collections::HashMap<i64, (Gd<#dto>, Gd<#dto>, Gd<#dto>, i64)>,
            #[base] base: Base<Node>,
        }

        impl #state {
            #snapshot_read
            pub fn upsert_from_data<C>(
                &mut self,
                entity_id: i64,
                data: &C::DataType,
                identity: &mut bevy_godot4::prelude::IdentitySubsystem,
                revision: i64,
            ) -> bool
            where
                C: bevy_godot4::prelude::DataTransferConfig<DtoType = #dto>,
                #dto: PartialEq,
            {
                use bevy_godot4::export_profile::{Stage as ExportStage, measure as measure_export};
                if entity_id < 0 {
                    return false;
                }

                // Keep allocation separate from looking up an existing triple of objects.
                let entry = measure_export(ExportStage::StoreLookup, || self.entries.entry(entity_id).or_insert_with(|| {
                    measure_export(ExportStage::ObjectCreation, || (<#dto>::new_gd(), <#dto>::new_gd(), <#dto>::new_gd(), -1))
                }));

                let (curr, prev, spare, updated_revision) = entry;

                if *updated_revision < 0 {
                    measure_export(ExportStage::FieldCopy, || C::update_dto(curr, data, identity));
                    measure_export(ExportStage::FieldCopy, || C::update_dto(prev, data, identity));
                    measure_export(ExportStage::FieldCopy, || C::update_dto(spare, data, identity));
                    *updated_revision = revision;
                    #snapshot_publish
                    self.change_source.bind_mut().mark(entity_id, revision);
                    return true;
                }

                measure_export(ExportStage::FieldCopy, || C::update_dto(spare, data, identity));

                let same_value = measure_export(ExportStage::Equality, || {
                    let curr_bound = curr.bind();
                    let spare_bound = spare.bind();
                    *curr_bound == *spare_bound
                });

                if same_value {
                    return false;
                }

                measure_export(ExportStage::Bookkeeping, || {
                    use std::mem::swap;
                    swap(prev, curr);
                    swap(curr, spare);
                    *updated_revision = revision;
                    #snapshot_publish
                    self.change_source.bind_mut().mark(entity_id, revision);
                });
                true
            }

            pub fn remove_entity(&mut self, entity_id: i64) {
                #snapshot_remove
                self.entries.remove(&entity_id);
                self.change_source.bind_mut().forget(entity_id);
            }

            /// Remove an optional capability while retaining its change notification.
            pub fn remove_optional(&mut self, entity_id: i64, revision: i64) {
                #snapshot_remove
                self.entries.remove(&entity_id);
                self.change_source.bind_mut().mark(entity_id, revision);
            }

            pub fn curr_for(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.entries
                    .get(&entity_id)
                    .map(|(curr, _prev, _spare, _revision)| curr.clone())
            }

            pub fn prev_for(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.entries
                    .get(&entity_id)
                    .map(|(_curr, prev, _spare, _revision)| prev.clone())
            }

            pub fn updated_revision_for(&self, entity_id: i64) -> i64 {
                self.change_source.bind().revision_for(entity_id)
            }
        }

        #[godot_api]
        impl #state {
            /// Receive this feature's current value after Bevy returns, or null on removal.
            /// The shared reader runs once per changed entity, including initial delivery.
            #[func(gd_self)]
            fn watch_values(
                this: Gd<Self>, host: Gd<Node>,
                entity: Gd<#meta_ident>, callback: Callable,
            ) -> Gd<#subscription_ident> {
                use bevy_godot4::prelude::{ExportValueBatch, GodotExportDelivery};
                let source = this.bind().change_source.clone();
                let mut delivery = GodotExportDelivery::resolve(&host)
                    .expect("value subscriber belongs to an authored Bevy app");
                let batch = delivery.bind_mut().value_batch(move || {
                    ExportValueBatch::new(&[source], move |id| {
                        if !this.is_instance_valid() { return None; }
                        Some(#prepared_ident(this.bind().curr_for(id)))
                    })
                });
                batch.watch(host, entity, callback)
            }

            /// Supply this source when declaring the features a visual needs.
            #[func]
            fn changes(&self) -> Gd<#change_source_ident> {
                self.change_source.clone()
            }
            #[func]
            fn resolve(owner: Gd<Node>) -> Option<Gd<#state>> {
                if !owner.is_instance_valid() {
                    return None;
                }

                let Ok(app) = bevy_godot4::prelude::BevyApp::resolve(&owner) else {
                    return None;
                };

                use bevy_godot4::prelude::child_services;
                let mut found = child_services::<Self>(app.upcast::<Node>());

                if found.len() > 1 {
                    panic!(
                        "Multiple {} nodes found under the owner node; expected exactly one singleton store",
                        stringify!(#state)
                    );
                }

                found.pop()
            }

            #[func]
            fn implements(&self, entity_id: i64) -> bool {
                self.entries.contains_key(&entity_id)
            }

            #[func]
            fn is_implemented_by_id(&self, entity_id: i64) -> bool {
                if entity_id < 0 {
                    return false;
                }

                self.implements(entity_id)
            }

            #[func]
            fn is_implemented_by(&self, entity_meta: Gd<bevy_godot4::prelude::EntityMeta>) -> bool {
                let Some(entity_id) = Self::entity_id_from_meta(&entity_meta) else {
                    return false;
                };

                self.implements(entity_id)
            }

            fn entity_id_from_meta(entity_meta: &Gd<bevy_godot4::prelude::EntityMeta>) -> Option<i64> {
                if !entity_meta.is_instance_valid() {
                    return None;
                }

                let entity_id = {
                    let bound = entity_meta.bind();
                    bound.entity_id
                };

                if entity_id < 0 {
                    return None;
                }

                Some(entity_id)
            }

            #[func]
            fn get_state(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.curr_for(entity_id)
            }

            #[func]
            fn get_curr(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.curr_for(entity_id)
            }

            #[func]
            fn get_prev(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.prev_for(entity_id)
            }

            #[func]
            fn is_updated_in_revision(&self, entity_id: i64, revision: i64) -> bool {
                self.updated_revision_for(entity_id) == revision
            }

            #[func]
            fn get_capability(entity_meta: Gd<bevy_godot4::prelude::EntityMeta>) -> Option<Gd<#state>> {
                let Some(entity_id) = Self::entity_id_from_meta(&entity_meta) else {
                    return None;
                };

                let store = Self::resolve(entity_meta.clone().upcast::<Node>())?;

                if store.bind().implements(entity_id) {
                    Some(store)
                } else {
                    None
                }
            }

        }

        #[godot_api]
        impl INode for #state {
            fn init(base: Base<Node>) -> Self {
                use bevy_godot4::prelude::register_store_on_entry;
                register_store_on_entry(&base);
                Self {
                    #snapshot_init
                    entries: std::collections::HashMap::new(),
                    change_source: #change_source_ident::new_gd(),
                    base,
                }
            }
        }

        impl #export_config_trait for #config {
            type StateNode = #state;
        }

        impl #export_state_trait<#config> for #state {
            fn resolve_store(host: Gd<Node>) -> Option<Gd<Self>> { Self::resolve(host) }
            fn change_source(&self) -> Gd<#change_source_ident> { self.changes() }
            fn read_current(&self, id: i64) -> Option<<#snapshot as #snapshots_ident<#dto>>::Value> {
                <#snapshot as #snapshots_ident<#dto>>::read_current(&self.snapshots, id, || self.curr_for(id))
            }
            fn read_values(&self, id: i64) -> Option<#pair_ident<<#snapshot as #snapshots_ident<#dto>>::Value>> {
                self.native_values_for(id)
            }
            fn read_dto(&self, id: i64) -> Option<Gd<#dto>> { self.curr_for(id) }
        }

        impl bevy_godot4::prelude::WithStateNode for #dto {
            type StateNode = #state;
        }
    };

    expanded.into()
}
