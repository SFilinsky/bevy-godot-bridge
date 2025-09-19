use proc_macro::TokenStream;
use heck::ToSnakeCase;

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


    let mut dto_field_idents: Vec<::syn::Ident> = Vec::new();
    let mut dto_field_types:  Vec<::syn::Type>  = Vec::new();

    for f in fields_iter {
        let mut include = false;
        for attr in &f.attrs {
            if attr.path().is_ident("export") {
                include = true;
                break;
            }
        }
        if include {
            let ident: ::syn::Ident = f.ident.clone().expect("named field");
            dto_field_idents.push(ident);
            dto_field_types.push(f.ty.clone());
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
                    #( d.#dto_field_idents = c.#dto_field_idents; )*
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