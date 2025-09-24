use proc_macro::TokenStream;
use heck::ToSnakeCase;
use syn::meta::ParseNestedMeta;
use syn::spanned::Spanned;

pub fn expand(input: TokenStream) -> TokenStream {
    use quote::{format_ident};

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

    fn option_inner(ty: &syn::Type) -> Option<&syn::Type> {
        if let syn::Type::Path(tp) = ty {
            if let Some(seg) = tp.path.segments.last() {
                if seg.ident == "Option" {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                            return Some(inner);
                        }
                    }
                }
            }
        }
        None
    }

    for f in fields_iter {
        let mut include   = false;
        let mut into_ty   : Option<syn::Type> = None;

        for attr in &f.attrs {
            if !attr.path().is_ident("export") { continue; }
            include = true;

            // #[export] or #[export(into(TargetType))]
            if let syn::Meta::List(list) = &attr.meta {
                let parser = syn::meta::parser(|meta: ParseNestedMeta| {
                    if meta.path.is_ident("into") {
                        meta.parse_nested_meta(|inner| {
                            let p = inner.path.clone();
                            let t = syn::Type::Path(syn::TypePath{ qself: None, path: p });
                            into_ty = Some(t);
                            Ok(())
                        })
                    } else {
                        Err(meta.error("supported options: into(TargetType)"))
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

            if let Some(target_ty) = into_ty {
                // Handle Option<T> → Option<Target>
                if let Some(inner) = option_inner(&src_ty) {
                    // Option<Src> -> Option<Target>, using &Src: Into<Target>
                    dto_field_types.push(syn::parse_quote!(Option<#target_ty>));

                    dto_assigns.push(quote::quote_spanned! { span=>
                    // Nice error if the conversion is missing:
                    const _: fn() = || {
                        fn _assert<S, D>() where for<'a> &'a S: ::core::convert::Into<D> {}
                        _assert::<#inner, #target_ty>();
                    };
                    d.#ident = c.#ident.as_ref().map(|v| ::core::convert::Into::<#target_ty>::into(v));
                });
                } else {
                    // Src -> Target, using &Src: Into<Target>
                    dto_field_types.push(target_ty.clone());

                    dto_assigns.push(quote::quote_spanned! { span=>
                    const _: fn() = || {
                        fn _assert<S, D>() where for<'a> &'a S: ::core::convert::Into<D> {}
                        _assert::<#src_ty, #target_ty>();
                    };
                    d.#ident = ::core::convert::Into::<#target_ty>::into(&c.#ident);
                });
                }
            } else {
                // No conversion: copy as-is
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
                app.add_systems(PostUpdate, #system_ident.as_visual_system());
            }
        }
    };

    expanded.into()
}