use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned, format_ident};
use syn::{parse_macro_input, spanned::Spanned, ItemStruct, Type};

pub fn expand(input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as ItemStruct);
    let tag_ident = &item.ident;
    let tag_snake = tag_ident.to_string().to_lowercase();

    // --- parse attribute -----------------------------------------------------
    let mut required: Vec<(Type, Span)> = vec![];
    let mut optional: Vec<(Type, Span)> = vec![];

    for attr in &item.attrs {
        if !attr.path().is_ident("export_entity") { continue; }

        match &attr.meta {
            syn::Meta::Path(_) => {
                // #[export_entity] — no args, fine
            }
            syn::Meta::List(list) => {
                // #[export_entity(required(A, B), optional(C, D))]
                let res = list.parse_args_with(syn::meta::parser(|meta: syn::meta::ParseNestedMeta| {
                    let name = meta.path.get_ident().map(|i| i.to_string());
                    match name.as_deref() {
                        Some("required") => meta.parse_nested_meta(|inner| {
                            let p = inner.path.clone();
                            let ty = Type::Path(syn::TypePath{ qself: None, path: p.clone() });
                            required.push((ty, p.span()));
                            Ok(())
                        }),
                        Some("optional") => meta.parse_nested_meta(|inner| {
                            let p = inner.path.clone();
                            let ty = Type::Path(syn::TypePath{ qself: None, path: p.clone() });
                            optional.push((ty, p.span()));
                            Ok(())
                        }),
                        _ => Err(meta.error("expected required(...) or optional(...)")),
                    }
                }));
                if let Err(e) = res { return e.to_compile_error().into(); }
            }
            syn::Meta::NameValue(nv) => {
                return syn::Error::new_spanned(nv, "unsupported: #[export_entity = ...]").to_compile_error().into();
            }
        }
    }

    // --- naming --------------------------------------------------------------
    fn field_ident_from_ty(ty: &Type) -> syn::Ident {
        let s = quote!(#ty).to_string();
        let last = s.split("::").last().unwrap();
        format_ident!("{}", heck::ToSnakeCase::to_snake_case(last))
    }

    let dto_ident          = format_ident!("{}Dto", tag_ident);
    let exporter_ident     = format_ident!("{}Exporter", tag_ident);
    let system_ident       = format_ident!("export_{}_changes", tag_snake);
    let export_plugin_ident = format_ident!("{}EntityExportPlugin", tag_ident);

    // required & optional lists split into (ty, span) + field idents
    let (req_tys, req_spans): (Vec<_>, Vec<_>) = required.iter().cloned().unzip();
    let (opt_tys, opt_spans): (Vec<_>, Vec<_>) = optional.iter().cloned().unzip();
    let req_fields: Vec<_> = req_tys.iter().map(field_ident_from_ty).collect();
    let opt_fields: Vec<_> = opt_tys.iter().map(field_ident_from_ty).collect();

    // updated filter: With<Tag> + OR of Changed<T> for all tracked comps
    let changed_terms: Vec<TokenStream2> = req_tys.iter().zip(req_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Changed<#ty>))
        .chain(opt_tys.iter().zip(opt_spans.iter())
            .map(|(ty, sp)| quote_spanned!(*sp=> Changed<#ty>)))
        .collect();

    let updated_filter: TokenStream2 = if changed_terms.len() == 1 {
        let only = &changed_terms[0];
        quote! { (With<#tag_ident>, #only) }
    } else {
        quote! { (With<#tag_ident>, Or<( #(#changed_terms),* )>) }
    };

    // tuple type for reading components: Option<&T> for both required and optional
    let req_read_types: Vec<_> = req_tys.iter().zip(req_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Option<& #ty>))
        .collect();
    let opt_read_types: Vec<_> = opt_tys.iter().zip(opt_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Option<& #ty>))
        .collect();

    let tuple_read_types: TokenStream2 = match (req_read_types.is_empty(), opt_read_types.is_empty()) {
        (true, true)   => quote! { () },
        (false, true)  => quote! { ( #(#req_read_types),* ) },
        (true, false)  => quote! { ( #(#opt_read_types),* ) },
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
    let req_field_types: Vec<_> = req_tys.iter().zip(req_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Gd<<#ty as ExportMeta>::Dto>))
        .collect();
    let opt_field_types: Vec<_> = opt_tys.iter().zip(opt_spans.iter())
        .map(|(ty, sp)| quote_spanned!(*sp=> Option<Gd<<#ty as ExportMeta>::Dto>>))
        .collect();

    // DTO constructor body (uses method form for nice clippy + type inference)
    let req_assigns: Vec<_> = req_vars.iter().zip(req_fields.iter())
        .map(|(var, field)| quote! { d.#field = (#var).to_dto(); })
        .collect();
    let opt_assigns: Vec<_> = opt_vars.iter().zip(opt_fields.iter()).zip(opt_spans.iter())
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
        (true,  true)  => quote! {},
        (false, true)  => quote! { #( #req_vars ),* },
        (true,  false) => quote! { #( #opt_vars ),* },
        (false, false) => quote! { #( #req_vars ),* , #( #opt_vars ),* },
    };

    // unwraps block (empty when no required)
    let req_unwraps_ts = if has_components { quote! { #(#req_unwraps)* } } else { quote! {} };

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
        use bevy_godot4::prelude::{SceneTreeRef, ExportMeta};
        use godot::classes::{Node, RefCounted};
        use godot::obj::{Base, Gd, NewGd};
        use godot::prelude::{GodotClass, godot_api};

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
            mut scene_tree: SceneTreeRef,
        ) {
            let Some(app) = scene_tree.get().get_root().unwrap().get_node_or_null("BevyAppSingleton") else { return; };
            let Some(exporter) = app.try_get_node_as::<#exporter_ident>(stringify!(#exporter_ident)) else { return; };

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
                app.add_systems(PostUpdate, #system_ident.as_visual_system());
            }
        }
    };

    expanded.into()
}