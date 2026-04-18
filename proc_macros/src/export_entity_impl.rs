use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, ItemStruct, Type};

pub fn expand(input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as ItemStruct);
    let tag_ident = &item.ident;
    let tag_snake = tag_ident.to_string().to_lowercase();

    // --- parse attribute -----------------------------------------------------
    let mut required: Vec<(Type, Span)> = vec![];
    let mut optional: Vec<(Type, Span)> = vec![];

    for attr in &item.attrs {
        if !attr.path().is_ident("export_entity") {
            continue;
        }

        match &attr.meta {
            syn::Meta::Path(_) => {
                // #[export_entity] — no args, fine
            }
            syn::Meta::List(list) => {
                // #[export_entity(required(A, B), optional(C, D))]
                let res =
                    list.parse_args_with(syn::meta::parser(|meta: syn::meta::ParseNestedMeta| {
                        let name = meta.path.get_ident().map(|i| i.to_string());
                        match name.as_deref() {
                            Some("required") => meta.parse_nested_meta(|inner| {
                                let p = inner.path.clone();
                                let ty = Type::Path(syn::TypePath {
                                    qself: None,
                                    path: p.clone(),
                                });
                                required.push((ty, p.span()));
                                Ok(())
                            }),
                            Some("optional") => meta.parse_nested_meta(|inner| {
                                let p = inner.path.clone();
                                let ty = Type::Path(syn::TypePath {
                                    qself: None,
                                    path: p.clone(),
                                });
                                optional.push((ty, p.span()));
                                Ok(())
                            }),
                            _ => Err(meta.error("expected required(...) or optional(...)")),
                        }
                    }));
                if let Err(e) = res {
                    return e.to_compile_error().into();
                }
            }
            syn::Meta::NameValue(nv) => {
                return syn::Error::new_spanned(nv, "unsupported: #[export_entity = ...]")
                    .to_compile_error()
                    .into();
            }
        }
    }

    // --- naming --------------------------------------------------------------
    fn field_ident_from_ty(ty: &Type) -> syn::Ident {
        let s = quote!(#ty).to_string();
        let last = s.split("::").last().unwrap();
        format_ident!("{}", heck::ToSnakeCase::to_snake_case(last))
    }

    let dto_ident = format_ident!("{}Dto", tag_ident);
    let exporter_ident = format_ident!("{}Exporter", tag_ident);
    let system_ident = format_ident!("export_{}_changes", tag_snake);
    let export_plugin_ident = format_ident!("{}EntityExportPlugin", tag_ident);
    let entity_spawn_handler_ident = format_ident!("{}EntitySpawnHandler", tag_ident);
    let entity_node_ident = format_ident!("{}Entity", tag_ident);

    // required & optional lists split into (ty, span) + field idents
    let (req_tys, req_spans): (Vec<_>, Vec<_>) = required.iter().cloned().unzip();
    let (opt_tys, opt_spans): (Vec<_>, Vec<_>) = optional.iter().cloned().unzip();
    let req_fields: Vec<_> = req_tys.iter().map(field_ident_from_ty).collect();
    let opt_fields: Vec<_> = opt_tys.iter().map(field_ident_from_ty).collect();

    // updated filter: With<Tag> + OR of Changed<T> for all tracked comps
    let changed_terms: Vec<TokenStream2> = req_tys
        .iter()
        .zip(req_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Changed<#ty>))
        .chain(
            opt_tys
                .iter()
                .zip(opt_spans.iter())
                .map(|(ty, sp)| quote_spanned!(*sp=> Changed<#ty>)),
        )
        .collect();

    // tuple type for reading components: Option<&T> for both required and optional
    let req_read_types: Vec<_> = req_tys
        .iter()
        .zip(req_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Option<& #ty>))
        .collect();
    let opt_read_types: Vec<_> = opt_tys
        .iter()
        .zip(opt_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Option<& #ty>))
        .collect();

    let tuple_read_types: TokenStream2 =
        match (req_read_types.is_empty(), opt_read_types.is_empty()) {
            (true, true) => quote! { () },
            (false, true) => quote! { ( #(#req_read_types),* ) },
            (true, false) => quote! { ( #(#opt_read_types),* ) },
            (false, false) => quote! { ( #(#req_read_types),* , #(#opt_read_types),* ) },
        };

    // destructuring vars (same names as fields)
    let req_vars = &req_fields;
    let opt_vars = &opt_fields;

    // required unwrapping with clear panic (keeps your current behavior)
    let req_unwraps: Vec<_> = req_tys.iter().zip(req_vars.iter())
        .map(|(ty, var)| quote! {
            let #var = match #var {
                Some(v) => v,
                None => panic!("Component [{}] is required on entity [{}]", stringify!(#ty), stringify!(#tag_ident)),
            };
        }).collect();

    // DTO field types using ExportMeta::Dto (spans preserved at type sites)
    let req_field_types: Vec<_> = req_tys
        .iter()
        .zip(req_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Gd<<#ty as ExportMeta>::Dto>))
        .collect();
    let opt_field_types: Vec<_> = opt_tys
        .iter()
        .zip(opt_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Option<Gd<<#ty as ExportMeta>::Dto>>))
        .collect();

    // DTO constructor body (uses method form for nice clippy + type inference)
    let req_assigns: Vec<_> = req_vars
        .iter()
        .zip(req_fields.iter())
        .map(|(var, field)| quote! { d.#field = (#var).to_dto(); })
        .collect();
    let opt_assigns: Vec<_> = opt_vars
        .iter()
        .zip(opt_fields.iter())
        .zip(opt_spans.iter())
        .map(|((var, field), sp)| quote_spanned!(*sp=> d.#field = #var.map(|c| c.to_dto()); ))
        .collect();

    // Type assertions to improve error messages when ExportMeta/GodotClass is missing.
    // These are zero-cost and ensure errors point at the attribute type tokens.
    let req_asserts: Vec<_> = req_tys.iter().zip(req_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> const _: fn() = || { fn _assert<T: ExportMeta + 'static>() {} _assert::<#ty>(); }; ))
        .collect();
    let opt_asserts: Vec<_> = opt_tys.iter().zip(opt_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> const _: fn() = || { fn _assert<T: ExportMeta + 'static>() {} _assert::<#ty>(); }; ))
        .collect();

    // -- expansion helpers ----------------------------------------------------

    let has_components = !(required.is_empty() && optional.is_empty());

    // pattern for `let Ok(<pattern>) = list.get(entity)`
    let tuple_destructure = if has_components {
        quote! { ( #( #req_vars ),* #( , #opt_vars )* ) }
    } else {
        quote! { () }
    };

    // args for `#dto_ident::from_components(<args>)`
    let from_args = match (required.is_empty(), optional.is_empty()) {
        (true, true) => quote! {},
        (false, true) => quote! { #( #req_vars ),* },
        (true, false) => quote! { #( #opt_vars ),* },
        (false, false) => quote! { #( #req_vars ),* , #( #opt_vars ),* },
    };

    // unwraps block (empty when no required)
    let req_unwraps_ts = if has_components {
        quote! { #(#req_unwraps)* }
    } else {
        quote! {}
    };

    // updated filter (fallback to Changed<Tag> when nothing to track)
    let updated_filter = if has_components {
        if changed_terms.len() == 1 {
            let only = &changed_terms[0];
            quote! { (With::<#tag_ident>, #only) }
        } else {
            quote! { (With::<#tag_ident>, Or<( #(#changed_terms),* )>) }
        }
    } else {
        quote! { (With::<#tag_ident>, Changed::<#tag_ident>) }
    };

    // DTO impl (empty vs populated)
    let dto_impl = if has_components {
        quote! {
            impl #dto_ident {
                pub fn from_components( #( #req_vars: & #req_tys ),* , #( #opt_vars: Option<& #opt_tys> ),* ) -> Gd<Self> {
                    #(#req_asserts)* #(#opt_asserts)*
                    let mut dto = Self::new_gd();
                    { let mut d = dto.bind_mut(); #(#req_assigns)* #(#opt_assigns)* }
                    dto
                }
            }
        }
    } else {
        quote! {
            impl #dto_ident {
                pub fn from_components() -> Gd<Self> { Self::new_gd() }
            }
        }
    };

    // --- expansion -----------------------------------------------------------
    let expanded = quote! {
        use bevy::prelude::{Added, App, Changed, Entity, Plugin, PostUpdate, Query, RemovedComponents, With, Or};
        use bevy_godot4::prelude::{BevyApp, ExportMeta, SceneTreeSubsystem};
        use godot::prelude::*;
        use std::collections::HashMap;


        // DTO -----------------------------------------------------------------
        #[derive(GodotClass)]
        #[class(init, base=RefCounted)]
        pub struct #dto_ident {
            #( #[var] pub #req_fields: #req_field_types, )*
            #( #[var] pub #opt_fields: #opt_field_types, )*
            #[base] base: Base<RefCounted>,
        }

        #dto_impl

        // Exporter Node -------------------------------------------------------
        #[derive(GodotClass)]
        #[class(init, base=Node)]
        pub struct #exporter_ident { #[base] base: Base<Node>, }

        #[godot_api]
        impl #exporter_ident {
            #[signal] fn created(entity_id: i64, dto: Gd<#dto_ident>);
            #[signal] fn updated(entity_id: i64, dto: Gd<#dto_ident>);
            #[signal] fn removed(entity_id: i64);
        }

        // System --------------------------------------------------------------
        pub fn #system_ident(
            created: Query<Entity, Added<#tag_ident>>,
            updated: Query<Entity, #updated_filter>,
            mut removed: RemovedComponents<#tag_ident>,
            list:   Query<#tuple_read_types, With<#tag_ident>>,
            app: BevyAppSubsystem,
        ) {
            let host = app.host_node();
            let Some(exporter) = host.try_get_node_as::<#exporter_ident>(stringify!(#exporter_ident)) else { return; };

            let mut created_ids: std::collections::HashSet<u64> = std::collections::HashSet::new();

            // Created
            for entity in created.iter() {
                created_ids.insert(entity.to_bits());
                let Ok(#tuple_destructure) = list.get(entity) else { continue; };
                #req_unwraps_ts
                let dto = #dto_ident::from_components(#from_args);
                exporter.signals().created().emit(entity.to_bits() as i64, &dto);
            }

            // Updated
            for entity in updated.iter() {
                if created_ids.contains(&entity.to_bits()) { continue; }
                let Ok(#tuple_destructure) = list.get(entity) else { continue; };
                #req_unwraps_ts
                let dto = #dto_ident::from_components(#from_args);
                exporter.signals().updated().emit(entity.to_bits() as i64, &dto);
            }

            // Removed
            for entity in removed.read() {
                exporter.signals().removed().emit(entity.to_bits() as i64);
            }
        }

        // Plugin --------------------------------------------------------------
        pub struct #export_plugin_ident;
        impl Plugin for #export_plugin_ident {
            fn build(&self, app: &mut App) {
                use bevy_godot4::prelude::AsVisualSystem;
                app.add_systems(PostUpdate, #system_ident);
            }
        }


        #[derive(GodotClass)]
        #[class(base = Node)]
        pub struct #entity_spawn_handler_ident {
            /// Scene that extends `#entity_node_ident`
            #[var]
            scene: Gd<PackedScene>,

            /// Optional parent; empty = this Node
            #[var]
            parent_path: NodePath,

            /// Cache of spawned nodes
            map: HashMap<i64, Gd<#entity_node_ident>>,

            /// Resolved parent for spawned runtime entities.
            parent_node_cache: Option<Gd<Node>>,

            /// Resolved `Exported` grouping node under parent host.
            exported_group_cache: Option<Gd<Node>>,

            #[base]
            base: Base<Node>,
        }

        #[godot_api]
        impl #entity_spawn_handler_ident {
            fn ensure_child_group(parent: &mut Gd<Node>, group_name: &str) -> Gd<Node> {
                if let Some(existing) = parent.try_get_node_as::<Node>(group_name) {
                    return existing;
                }

                let mut node = Node::new_alloc();
                node.set_name(group_name);
                parent.add_child(&node);
                node
            }

            fn parent_node(&mut self) -> Option<Gd<Node>> {
                if let Some(parent) = self.parent_node_cache.as_ref() {
                    if parent.is_instance_valid() {
                        if !self.parent_path.is_empty() {
                            return Some(parent.clone());
                        }

                        if let Some(group) = self.exported_group_cache.as_ref() {
                            if group.is_instance_valid() {
                                return Some(group.clone());
                            }

                            godot_error!(
                                "{} exported grouping node became invalid after initial resolve",
                                stringify!(#entity_spawn_handler_ident)
                            );
                            return None;
                        }

                        let mut host = parent.clone();
                        let group = Self::ensure_child_group(&mut host, "Exported");
                        self.exported_group_cache = Some(group.clone());
                        return Some(group);
                    }

                    godot_error!(
                        "{} parent node became invalid after initial resolve",
                        stringify!(#entity_spawn_handler_ident)
                    );
                    return None;
                }

                godot_error!(
                    "{} parent node was not resolved during ready()",
                    stringify!(#entity_spawn_handler_ident)
                );
                None
            }

            #[func]
            fn _on_created(&mut self, entity_id: i64, dto: Gd<#dto_ident>) {
                // decide parent
                let Some(mut parent) = self.parent_node() else {
                    return;
                };

                // instance + add
                let inst = self.scene.instantiate().unwrap();
                parent.add_child(&inst);

                // downcast to typed entity
                let Ok(mut ent) = inst.clone().try_cast::<#entity_node_ident>() else {
                    godot_error!("Spawned scene must extend {}", stringify!(#entity_node_ident));
                    return;
                };

                ent.bind_mut().entity_id = entity_id;

                // apply dto (defer if not yet in tree)
                if inst.is_inside_tree() {
                    ent.bind_mut().apply_dto(dto);
                } else {
                    ent.call_deferred("apply_dto", &[Variant::from(dto)]);
                }

                self.map.insert(entity_id, ent);
            }

            #[func]
            fn _on_updated(&mut self, entity_id: i64, dto: Gd<#dto_ident>) {
                if let Some(ent) = self.map.get_mut(&entity_id) {
                    ent.bind_mut().apply_dto(dto);
                }
            }

            #[func]
            fn _on_removed(&mut self, entity_id: i64) {
                if let Some(ent) = self.map.remove(&entity_id) {
                    if ent.is_instance_valid() { ent.upcast::<Node>().queue_free(); }
                }
            }
        }

        #[godot_api]
        impl INode for #entity_spawn_handler_ident {
            fn init(base: Base<Node>) -> Self {
                Self {
                    scene: PackedScene::new_gd(),
                    parent_path: NodePath::default(),
                    map: HashMap::new(),
                    parent_node_cache: None,
                    exported_group_cache: None,
                    base,
                }
            }

            fn ready(&mut self) {
                let host = self.base().clone().upcast::<Node>();
                let Ok(app) = BevyApp::resolve(&host) else {
                    godot_error!("BevyApp not found for {}", stringify!(#entity_spawn_handler_ident)); return;
                };
                let Some(mut exporter) = app.try_get_node_as::<Node>(stringify!(#exporter_ident)) else {
                    godot_warn!("{} not found under resolved BevyApp", stringify!(#exporter_ident)); return;
                };

                if !self.parent_path.is_empty() {
                    self.parent_node_cache = Some(self.base().get_node_as::<Node>(&self.parent_path));
                } else {
                    self.parent_node_cache = Some(app.bind().resolve_node_host());
                }

                let on_created = self.base().callable("_on_created");
                let on_updated = self.base().callable("_on_updated");
                let on_removed = self.base().callable("_on_removed");

                let _ = exporter.connect("created", &on_created);
                let _ = exporter.connect("updated", &on_updated);
                let _ = exporter.connect("removed", &on_removed);
            }
        }

        #[derive(GodotClass)]
        #[class(base = Node)]
        pub struct #entity_node_ident {
            #[var]
            pub entity_id: i64,
            #[base]
            base: Base<Node>,
        }

        #[godot_api]
        impl #entity_node_ident {
            #[func]
            fn apply_dto(&mut self, dto: Gd<#dto_ident>) {
                self.signals().on_update().emit(&dto);
            }

            #[signal]
            fn on_update(dto: Gd<#dto_ident>);
        }

        #[godot_api]
        impl INode for #entity_node_ident {
            fn init(base: Base<Node>) -> Self {
                Self { entity_id: -1, base }
            }
        }
    };

    expanded.into()
}
