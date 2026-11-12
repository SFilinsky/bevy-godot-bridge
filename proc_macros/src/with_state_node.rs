//! Code used by `with_state_node!`.
//!
//! This macro creates a Godot node for one kind of exported entity data. The
//! node stores the current and previous values. It also lets Godot ask, with a
//! normal typed call, whether an entity has this feature.

use heck::ToSnakeCase;
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
    let module = format_ident!("{}_publication", state.to_string().to_snake_case());
    let config_path = if config.leading_colon.is_some()
        || config
            .segments
            .first()
            .is_some_and(|part| part.ident == "crate")
    {
        quote!(#config)
    } else {
        quote!(super::#config)
    };
    let config: Path = syn::parse_quote!(Config);
    let config_trait = format_ident!("{state}TransferConfig");
    let prepared = format_ident!("{state}PreparedValue");
    // Tests can inspect removal versions without adding a gameplay-facing method.
    let test_revision_read = cfg!(feature = "bridge-test-api").then(|| {
        quote! {
            #[func]
            fn is_updated_in_revision(&self, id: i64, revision: i64) -> bool {
                self.change_source.bind().revision_for(id) == revision
            }
        }
    });
    let expanded = quote! {
        use godot::prelude::*;
        use godot::classes::Engine;
        use godot::meta::ToGodot;
        use bevy_godot4::prelude::{DtoOf, DataTransferConfig as #config_trait, ExportConfig, ExportState,
            ExportChangeSource, ExportPublication, UpdateDispatcher, StatePublication, SnapshotPair,
            ExportValue, UpdatePayload, UpdateReceiver, EntityMeta, UpdateSubscription, ExportValueBatch, IdentitySubsystem,
            register_store_on_entry, child_services, BevyApp, StateReadError, report_state_error};
        use std::cell::RefCell;
        use std::rc::Rc;
        use bevy_godot4::export::profiling::{Stage as ExportStage, measure as measure_export};

        #[derive(Clone)]
        struct #prepared(Option<<#config as #config_trait>::Snapshot>);
        impl ExportValue for #prepared {
            const NAME: &'static str = stringify!(#state);
        }
        impl UpdatePayload for #prepared {
            type Fields = (Option<<#config as #config_trait>::Snapshot>,);
            fn into_fields(self) -> Self::Fields { (self.0,) }
            fn write_arguments(&self, arguments: &mut Array<Variant>) {
                arguments.clear();
                arguments.push(&self.0.as_ref().map(ToGodot::to_godot_owned).to_variant());
            }
        }

        #[derive(GodotClass)]
        #[class(base=Node)]
        pub struct #state {
            publication: Rc<RefCell<StatePublication<<#config as #config_trait>::Snapshot>>>,
            registration: Option<Rc<ExportPublication>>,
            change_source: Gd<ExportChangeSource>,
            #[base] base: Base<Node>,
        }

        impl #state {
            pub fn upsert_from_data<C>(&mut self, entity_id: i64, data: &C::DataType, identity: &mut IdentitySubsystem, revision: i64) -> bool
            where C: #config_trait<Snapshot = <#config as #config_trait>::Snapshot> {
                // Copy source data before queuing; do not create or mutate a Godot DTO.
                let value = measure_export(ExportStage::FieldCopy, || C::snapshot_from_data(data, identity));
                self.enqueue_snapshot(entity_id, value, revision)
            }
            pub fn enqueue_snapshot(&mut self, id: i64, value: <#config as #config_trait>::Snapshot, revision: i64) -> bool {
                let changed = measure_export(ExportStage::Equality, || self.publication.borrow_mut().enqueue(id, value, revision));
                if changed { self.request_publication(); }
                changed
            }
            pub fn remove_entity(&mut self, id: i64) { self.publication.borrow_mut().remove(id, -1, true); self.request_publication(); }
            pub fn remove_optional(&mut self, id: i64, revision: i64) { self.publication.borrow_mut().remove(id, revision, false); self.request_publication(); }
            fn request_publication(&self) { if let Some(registration) = &self.registration { registration.request(); } }
            pub fn queued_implements(&self, id: i64) -> bool { self.publication.borrow().queued_contains(id) }
            pub fn curr_for(&self, id: i64) -> Option<Gd<DtoOf<#config>>> { self.publication.borrow().current_dto(id) }
            pub fn prev_for(&self, id: i64) -> Option<Gd<DtoOf<#config>>> { self.publication.borrow().previous_dto(id) }
            pub fn native_values_for(&self, id: i64) -> Option<SnapshotPair<<#config as #config_trait>::Snapshot>> { self.publication.borrow().pair(id) }

        }

        #[godot_api]
        impl #state {
            #[func]
            pub fn subscribe(&self, host: Gd<Node>, entity: Gd<EntityMeta>, receiver: Gd<UpdateReceiver>) -> Option<Gd<UpdateSubscription>> {
                let id = entity.bind().entity_id;
                self.subscribe_for(host, id, Some(entity), receiver)
            }
            #[func]
            pub fn subscribe_id(&self, host: Gd<Node>, id: i64, receiver: Gd<UpdateReceiver>) -> Option<Gd<UpdateSubscription>> {
                self.subscribe_for(host, id, None, receiver)
            }
            fn subscribe_for(&self, host: Gd<Node>, id: i64, entity: Option<Gd<EntityMeta>>, receiver: Gd<UpdateReceiver>) -> Option<Gd<UpdateSubscription>> {
                // Reject a wiring mistake immediately, before saving a receiver.
                if !self.implements(id) {
                    report_state_error(stringify!(#state), id, &StateReadError::missing::<#config>());
                    return None;
                }
                let mut delivery = UpdateDispatcher::resolve(&host).expect("state subscription needs an authored UpdateDispatcher");
                let source = self.change_source.clone();
                let publication = Rc::downgrade(&self.publication);
                let batch = delivery.bind_mut().value_batch(move || ExportValueBatch::new(&[source], move |id| {
                    let publication = publication.upgrade()?;
                    let value = publication.borrow().current(id);
                    Some(#prepared(value))
                }));
                batch.subscribe_for(host, id, entity, receiver)
            }
            #[func]
            pub fn resolve(owner: Gd<Node>) -> Option<Gd<Self>> {
                if !owner.is_instance_valid() { return None; }
                let app = BevyApp::resolve(&owner).ok()?;
                let mut found = child_services::<Self>(app.upcast::<Node>());
                assert!(found.len() <= 1, "author one store per feature");
                found.pop()
            }
            #[func]
            pub fn implements(&self, id: i64) -> bool { self.publication.borrow().contains(id) }
            #[func]
            fn get_current(&self, id: i64) -> Option<Gd<DtoOf<#config>>> { self.curr_for(id) }
            #[func]
            fn get_previous(&self, id: i64) -> Option<Gd<DtoOf<#config>>> { self.prev_for(id) }
            #test_revision_read

        }

        #[godot_api]
        impl INode for #state {
            fn init(base: Base<Node>) -> Self {
                register_store_on_entry(&base);
                Self { publication: Rc::new(RefCell::new(StatePublication::default())), registration: None, change_source: ExportChangeSource::new_gd(), base }
            }
            fn ready(&mut self) {
                if Engine::singleton().is_editor_hint() { return; }
                let mut delivery = UpdateDispatcher::resolve(&self.base().clone()).expect("state store needs authored delivery");
                let publication = Rc::downgrade(&self.publication);
                let source = self.change_source.clone();
                let changed = RefCell::new(Vec::new());
                self.registration = Some(delivery.bind_mut().register_publication(false, move || {
                    let Some(publication) = publication.upgrade() else { return; };
                    let mut changed = changed.borrow_mut();
                    publication.borrow_mut().publish(&mut changed);
                    let mut source_handle = source.clone();
                    let mut source = source_handle.bind_mut();
                    for (id, revision, forget) in changed.drain(..) {
                        if forget { source.forget(id); } else { source.mark(id, revision); }
                    }
                }));
            }
            fn exit_tree(&mut self) {
                self.registration = None;
                self.publication = Rc::new(RefCell::new(StatePublication::default()));
                self.change_source = ExportChangeSource::new_gd();
                self.base_mut().request_ready();
            }
        }
        impl ExportConfig for #config { type StateNode = #state; }
        impl ExportState<#config> for #state {
            fn resolve_store(host: Gd<Node>) -> Option<Gd<Self>> { Self::resolve(host) }
            fn change_source(&self) -> Gd<ExportChangeSource> { self.change_source.clone() }
            fn read_current(&self, id: i64) -> Option<<#config as #config_trait>::Snapshot> { self.publication.borrow().current(id) }
            fn read_values(&self, id: i64) -> Option<SnapshotPair<<#config as #config_trait>::Snapshot>> { self.native_values_for(id) }
        }
    };
    quote! { mod #module { type Config = #config_path; #expanded } pub use #module::#state; }.into()
}
