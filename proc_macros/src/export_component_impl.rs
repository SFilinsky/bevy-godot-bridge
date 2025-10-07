use proc_macro::TokenStream;
use heck::ToSnakeCase;
use syn::spanned::Spanned;

pub fn expand(input: TokenStream) -> TokenStream {
    use quote::{format_ident};
    use syn::{Type, TypePath, PathArguments, GenericArgument};

    let item: ::syn::ItemStruct = ::syn::parse_macro_input!(input);


    let comp_ident = &item.ident;                     // e.g. Health
    let comp_snake = comp_ident.to_string().to_snake_case(); // "health"

    let dto_ident     = format_ident!("{}Dto", comp_ident);          // HealthDto
    let exporter_ident= format_ident!("{}Exporter", comp_ident);     // HealthExporter
    let system_ident  = format_ident!("export_{}_changes", comp_snake); // export_health_changes
    let plugin_ident  = format_ident!("{}ExportPlugin", comp_ident); // HealthExportPlugin (if you emit one)


    let fields_iter: &::syn::punctuated::Punctuated<::syn::Field, ::syn::token::Comma> = match &item.fields {
        ::syn::Fields::Named(named) => &named.named,
        _ => {
            return ::syn::Error::new(
                ::syn::spanned::Spanned::span(&item),
                "export only supports structs with named fields",
            )
                .to_compile_error()
                .into();
        }
    };


    let mut dto_field_idents: Vec<syn::Ident> = Vec::new();
    let mut dto_field_types:  Vec<syn::Type>  = Vec::new();
    let mut dto_assigns:      Vec<proc_macro2::TokenStream> = Vec::new();

    fn option_inner(ty: &syn::Type) -> Option<syn::Type> {
        let Type::Path(TypePath { qself: None, path }) = ty else {
            return None;
        };
        let seg = path.segments.last()?;
        if seg.ident != "Option" {
            return None;
        }
        let PathArguments::AngleBracketed(args) = &seg.arguments else {
            return None;
        };
        if args.args.len() != 1 {
            return None;
        }
        match args.args.first().unwrap() {
            GenericArgument::Type(inner) => Some(inner.clone()),
            _ => None,
        }
    }

    for f in fields_iter {
        let mut include   = false;
        let mut into_ty   : Option<syn::Type> = None;

        for attr in &f.attrs {
            if !attr.path().is_ident("export") { continue; }
            include = true;

            if let syn::Meta::List(list) = &attr.meta {
                let parser = syn::meta::parser(|meta: syn::meta::ParseNestedMeta| {
                    if meta.path.is_ident("into") {
                        // Support: #[export(into = Type)]  and  #[export(into(Type))]
                        if meta.input.peek(syn::Token![=]) {
                            let _eq: syn::Token![=] = meta.input.parse()?;
                            let ty: syn::Type = meta.input.parse()?;
                            into_ty = Some(ty);
                            Ok(())
                        } else {
                            meta.parse_nested_meta(|inner| {
                                let ty: syn::Type = inner.input.parse()?;
                                into_ty = Some(ty);
                                Ok(())
                            })
                        }
                    } else {
                        Err(meta.error("supported options: into(<Type>) or into = <Type>"))
                    }
                });
                if let Err(e) = syn::parse::Parser::parse2(parser, list.tokens.clone()) {
                    return e.to_compile_error().into();
                }
            }
        }

        if include {
            let ident = f.ident.clone().expect("named field");
            let src_ty = f.ty.clone();
            let span   = src_ty.span();

            dto_field_idents.push(ident.clone());

            if let Some(target_ty) = into_ty.clone() {
                let src_is_opt  = option_inner(&src_ty);          // Option<S>?
                let dst_is_opt  = option_inner(&target_ty);       // Option<T>?

                match (src_is_opt, dst_is_opt) {
                    // Option<S> -> Option<T>
                    (Some(src_inner), Some(dst_inner)) => {
                        // Disallow Option<GString> (and guide the user)
                        if let syn::Type::Path(tp) = &dst_inner {
                            if tp.path.segments.last().map(|s| s.ident == "GString").unwrap_or(false) {
                                return syn::Error::new(
                                    span,
                                    "Option<GString> is not supported by godot-rust. \
                     Use #[export(into(GString))] instead; None will map to empty string."
                                ).to_compile_error().into();
                            }
                        }

                        dto_field_types.push(syn::parse_quote!(Option<#dst_inner>));
                        dto_assigns.push(quote::quote_spanned! { span=>
                            const _: fn() = || {
                                fn _assert<S, D>() where for<'a> &'a S: ::core::convert::Into<D> {}
                                _assert::<#src_inner, #dst_inner>();
                            };
                            d.#ident = c.#ident.as_ref().map(|v| ::core::convert::Into::<#dst_inner>::into(v));
                        });
                    }

                    // Option<S> -> T   (we export as T and map None -> T::default())
                    (Some(src_inner), None) => {
                        dto_field_types.push(target_ty.clone());
                        dto_assigns.push(quote::quote_spanned! { span=>
                            const _: fn() = || {
                                fn _assert_into<S, D>() where for<'a> &'a S: ::core::convert::Into<D> {}
                                fn _assert_default<D: ::core::default::Default>() {}
                                _assert_into::<#src_inner, #target_ty>();
                                _assert_default::<#target_ty>();
                            };
                            d.#ident = c.#ident
                                .as_ref()
                                .map(|v| ::core::convert::Into::<#target_ty>::into(v))
                                .unwrap_or_default();
                        });
                    }

                    // S -> Option<T>   (wrap in Some)
                    (None, Some(dst_inner)) => {
                        // Same guard for Option<GString>
                        if let syn::Type::Path(tp) = &dst_inner {
                            if tp.path.segments.last().map(|s| s.ident == "GString").unwrap_or(false) {
                                return syn::Error::new(
                                    span,
                                    "Option<GString> is not supported by godot-rust."
                                ).to_compile_error().into();
                            }
                        }

                        dto_field_types.push(syn::parse_quote!(Option<#dst_inner>));
                        dto_assigns.push(quote::quote_spanned! { span=>
                            const _: fn() = || {
                                fn _assert<S, D>() where for<'a> &'a S: ::core::convert::Into<D> {}
                                _assert::<#src_ty, #dst_inner>();
                            };
                            d.#ident = Some(::core::convert::Into::<#dst_inner>::into(&c.#ident));
                        });
                    }

                    // S -> T
                    (None, None) => {
                        dto_field_types.push(target_ty.clone());
                        dto_assigns.push(quote::quote_spanned! { span=>
                            const _: fn() = || {
                                fn _assert<S, D>() where for<'a> &'a S: ::core::convert::Into<D> {}
                                _assert::<#src_ty, #target_ty>();
                            };
                            d.#ident = ::core::convert::Into::<#target_ty>::into(&c.#ident);
                        });
                    }
                }
            } else {
                // No conversion: copy as-is (works for Copy fields; otherwise require Clone or adjust)
                dto_field_types.push(src_ty.clone());
                dto_assigns.push(quote::quote! { d.#ident = c.#ident; });
            }
        }
    }

    let expanded = ::quote::quote! {
        use bevy_godot4::godot::obj::NewGd;
        use bevy_godot4::godot::prelude::{Base, Gd, GodotClass, RefCounted, Node, godot_api};
        use bevy::prelude::{App, Added, Changed, Entity, Query, RemovedComponents, PostUpdate, Plugin};
        use std::collections::HashSet;
        use bevy_godot4::{collect_children, ExportMeta};
        use bevy_godot4::prelude::{SceneTreeRef, AsVisualSystem};

        impl ExportMeta for #comp_ident {
            type Dto = #dto_ident;
            fn to_dto(&self) -> Gd<Self::Dto> {
                #dto_ident::from_component(self)
            }
        }

        #[derive(GodotClass)]
        #[class(init, base=RefCounted)]
        pub struct #dto_ident {
            #( #[var] pub #dto_field_idents: #dto_field_types, )*
            #[base]
            base: Base<RefCounted>,
        }

        impl #dto_ident {
            pub fn from_component(c: &#comp_ident) -> Gd<Self> {
                let mut dto = Self::new_gd();
                {
                    let mut d = dto.bind_mut();
                    #( #dto_assigns )*
                }
                dto
            }
        }

        #[derive(GodotClass)]
        #[class(init, base=Node)]
        pub struct #exporter_ident {
            #[base]
            base: Base<Node>,
        }

        #[godot_api]
        impl #exporter_ident {
            #[signal] fn created(entity_id: i64, dto: Gd<#dto_ident>);
            #[signal] fn updated(entity_id: i64, dto: Gd<#dto_ident>);
            #[signal] fn removed(entity_id: i64);
        }

        #[allow(non_snake_case)]
        fn #system_ident(
            created: Query<(Entity, &#comp_ident), Added<#comp_ident>>,
            updated: Query<(Entity, &#comp_ident), Changed<#comp_ident>>,
            mut removed: RemovedComponents<#comp_ident>,
            mut scene_tree: SceneTreeRef,
        ) {
            // Scope to the current BevyAppSingleton host
            let Some(mut host) = scene_tree
                .get()
                .get_root()
                .unwrap()
                .get_node_or_null("BevyAppSingleton")
            else { return; };

            // Find all exporters of this type under the host
            let mut exporters = collect_children::<#exporter_ident>(host, true);
            if exporters.is_empty() { return; }

            // Avoid double "updated" on insert frames
            let mut created_ids: HashSet<u64> = HashSet::new();

            // Created
            for (entity, comp) in created.iter() {
                created_ids.insert(entity.to_bits());
                let dto = #dto_ident::from_component(comp);
                for exporter in exporters.iter_mut() {
                    exporter
                        .signals()
                        .created()
                        .emit(entity.to_bits() as i64, &dto);
                }
            }

            // Updated
            for (entity, comp) in updated.iter() {
                if created_ids.contains(&entity.to_bits()) {
                    continue;
                }
                let dto = #dto_ident::from_component(comp);
                for exporter in exporters.iter_mut() {
                    exporter
                        .signals()
                        .updated()
                        .emit(entity.to_bits() as i64, &dto);
                }
            }

            // Removed
            for entity in removed.read() {
                for exporter in exporters.iter_mut() {
                    exporter.signals().removed().emit(entity.to_bits() as i64);
                }
            }
        }

        pub struct #plugin_ident;
        impl Plugin for #plugin_ident {
            fn build(&self, app: &mut App) {
                app.add_systems(PostUpdate, #system_ident);
            }
        }
    };

    expanded.into()
}