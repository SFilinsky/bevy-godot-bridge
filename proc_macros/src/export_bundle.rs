use heck::ToSnakeCase;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    Ident, LitStr, Path, Result, Token, Type, bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// Usage:
/// export_bundle!{
///   name: "Unit",
///   tag: UnitTag,
///   components: [ TransformExportConfig, FactionExportConfig, Option<MovementExportConfig> ],
/// }
///
/// Each `*ExportConfig` implements:
/// - bevy_godot4::prelude::DataTransferConfig
///   - type DataType = <bevy component>
///   - type DtoType = <GodotClass DTO>
///   - update_dto / update_data
struct Spec {
    name: LitStr,
    tag: Path,
    items: Vec<Item>,
}

enum Item {
    Required(Type), // FooExportConfig
    Optional(Type), // Option<FooExportConfig>
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name: Option<LitStr> = None;
        let mut tag: Option<Path> = None;
        let mut items: Option<Vec<Item>> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "name" => name = Some(input.parse()?),
                "tag" => tag = Some(input.parse()?),
                "components" => {
                    let list;
                    bracketed!(list in input);

                    let mut v: Vec<Item> = Vec::new();
                    while !list.is_empty() {
                        let ty: Type = list.parse()?;
                        let ts = quote!(#ty).to_string();
                        if ts.starts_with("Option <") || ts.starts_with("Option<") {
                            v.push(Item::Optional(ty));
                        } else {
                            v.push(Item::Required(ty));
                        }
                        if list.peek(Token![,]) {
                            list.parse::<Token![,]>()?;
                        } else {
                            break;
                        }
                    }
                    items = Some(v);
                }
                other => {
                    return Err(syn::Error::new_spanned(
                        key,
                        format!("unknown key `{other}`; expected `name`, `tag`, or `components`"),
                    ));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        let name = name.ok_or_else(|| syn::Error::new(input.span(), "missing `name: ...`"))?;
        let tag = tag.ok_or_else(|| syn::Error::new(input.span(), "missing `tag: ...`"))?;
        let items =
            items.ok_or_else(|| syn::Error::new(input.span(), "missing `components: [ ... ]`"))?;

        Ok(Spec { name, tag, items })
    }
}

/// Derive field name from meta/config type name:
/// TransformExportConfig -> transform
/// FactionTransferConfig -> faction
fn dto_field_ident_from_cfg(cfg_ty: &Type) -> Ident {
    let raw = quote!(#cfg_ty).to_string();

    // Strip Option<...>
    let base = if raw.starts_with("Option <") || raw.starts_with("Option<") {
        let cut = raw.trim();
        let inner = &cut[cut.find('<').unwrap() + 1..cut.rfind('>').unwrap()];
        inner.trim().split("::").last().unwrap_or(inner).to_string()
    } else {
        raw.trim()
            .split("::")
            .last()
            .unwrap_or(raw.trim())
            .to_string()
    };

    let base = base
        .strip_suffix("TransferConfig")
        .unwrap_or(&base)
        .to_string();

    format_ident!("{}", base.to_snake_case())
}

// --- helpers ---------------------------------------------------------------
fn strip_option(ty: &Type) -> Type {
    if let Type::Path(tp) = ty {
        if let Some(seg) = tp.path.segments.last() {
            if seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(ab) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(inner)) = ab.args.first() {
                        return inner.clone();
                    }
                }
            }
        }
    }
    ty.clone()
}

fn relative_type_path(ty: &syn::Type) -> Option<syn::Path> {
    if let syn::Type::Path(tp) = ty {
        let p = &tp.path;
        if p.leading_colon.is_none() {
            return Some(p.clone());
        }
    }
    None
}

fn relative_path(path: &syn::Path) -> Option<syn::Path> {
    if path.leading_colon.is_none() {
        Some(path.clone())
    } else {
        None
    }
}

pub fn expand(input: TokenStream) -> TokenStream {
    // -------------------- PARSE --------------------
    let spec: Spec = parse_macro_input!(input as Spec);

    let name_str = spec.name.value();
    let base_ident = format_ident!("{}", name_str);
    let base_snake = name_str.to_snake_case();

    // -------------------- TRANSFORM --------------------
    let wrapper_module_ident = format_ident!("{}_export", base_snake);
    let wrapper_dto_ident = format_ident!("{}EntityDto", base_ident);
    let exporter_ident = format_ident!("{}EntityExporter", base_ident);
    let plugin_ident = format_ident!("{}EntityExportPlugin", base_ident);
    let system_ident = format_ident!("export_{}_entity_changes", base_snake);
    let entity_spawn_handler_ident = format_ident!("{}EntitySpawnHandler", base_ident);
    let entity_node_ident = format_ident!("{}Entity", base_ident);
    let updates_ident = format_ident!("{}EntityUpdateInfo", base_ident);
    let exporter_accessor_ident = format_ident!("{}ExporterAccessor", base_ident);
    let exporter_accessor_impl_ident = format_ident!("{}ExporterAccessorImpl", base_ident);
    let batch_ident = format_ident!("{}EntityBatch", base_ident);

    // Split items into required/optional config types
    let mut required_type_list: Vec<Type> = Vec::new();
    let mut optional_type_list: Vec<Type> = Vec::new();
    for it in spec.items.iter() {
        match it {
            Item::Required(ty) => required_type_list.push(strip_option(ty)),
            Item::Optional(ty) => optional_type_list.push(strip_option(ty)),
        }
    }

    // Field idents derived from config names
    let required_field_list: Vec<Ident> = required_type_list
        .iter()
        .map(dto_field_ident_from_cfg)
        .collect();
    let optional_field_list: Vec<Ident> = optional_type_list
        .iter()
        .map(dto_field_ident_from_cfg)
        .collect();

    let required_field_list0: Vec<Ident> = required_field_list
        .iter()
        .map(|f| format_ident!("{}0", f))
        .collect();
    let required_field_list1: Vec<Ident> = required_field_list
        .iter()
        .map(|f| format_ident!("{}1", f))
        .collect();
    let required_field_list2: Vec<Ident> = required_field_list
        .iter()
        .map(|f| format_ident!("{}2", f))
        .collect();

    let optional_field_list0: Vec<Ident> = optional_field_list
        .iter()
        .map(|f| format_ident!("{}0", f))
        .collect();
    let optional_field_list1: Vec<Ident> = optional_field_list
        .iter()
        .map(|f| format_ident!("{}1", f))
        .collect();
    let optional_field_list2: Vec<Ident> = optional_field_list
        .iter()
        .map(|f| format_ident!("{}2", f))
        .collect();

    // Associated types
    let required_component_type_list: Vec<TokenStream2> = required_type_list
        .iter()
        .map(|c| quote! { <#c as DataTransferConfig>::DataType })
        .collect();
    let optional_component_type_list: Vec<TokenStream2> = optional_type_list
        .iter()
        .map(|c| quote! { <#c as DataTransferConfig>::DataType })
        .collect();

    let required_dto_type_list: Vec<TokenStream2> = required_type_list
        .iter()
        .map(|c| quote! { <#c as DataTransferConfig>::DtoType })
        .collect();
    let optional_dto_type_list: Vec<TokenStream2> = optional_type_list
        .iter()
        .map(|c| quote! { <#c as DataTransferConfig>::DtoType })
        .collect();

    // All fields (req then opt)
    let all_field_list: Vec<Ident> = required_field_list
        .iter()
        .cloned()
        .chain(optional_field_list.iter().cloned())
        .collect();

    // Filters
    let tag_ty = &spec.tag;
    let created_filter = quote! { Added<#tag_ty> };

    let changed_terms: Vec<_> = required_component_type_list
        .iter()
        .chain(optional_component_type_list.iter())
        .map(|c| quote! { Changed<#c> })
        .collect();

    let updated_filter = if changed_terms.is_empty() {
        quote! { With::<#tag_ty> }
    } else if changed_terms.len() == 1 {
        let only = &changed_terms[0];
        quote! { (With::<#tag_ty>, #only) }
    } else {
        quote! { (With::<#tag_ty>, Or<( #( #changed_terms ),* )>) }
    };

    let removed_decl = quote! { mut removed: RemovedComponents<#tag_ty>, };

    // Snapshot tuple
    let read_types: Vec<_> = required_component_type_list
        .iter()
        .map(|c| quote! { Option<& #c> })
        .chain(
            optional_component_type_list
                .iter()
                .map(|c| quote! { Option<& #c> }),
        )
        .collect();

    let read_tuple: TokenStream2 = if read_types.is_empty() {
        quote! { () }
    } else {
        quote! { ( #( #read_types, )* ) }
    };

    let read_vars: Vec<Ident> = all_field_list
        .iter()
        .map(|f| format_ident!("__{}", f))
        .collect();

    let destructure = if read_vars.is_empty() {
        quote! { () }
    } else {
        quote! { ( #( #read_vars, )* ) }
    };

    // SnapshotRust fields
    let snapshot_req_field_types: Vec<TokenStream2> = required_component_type_list
        .iter()
        .map(|c| quote! { #c })
        .collect();
    let snapshot_opt_field_types: Vec<TokenStream2> = optional_component_type_list
        .iter()
        .map(|c| quote! { Option<#c> })
        .collect();

    let snapshot_req_fields: Vec<TokenStream2> = required_field_list
        .iter()
        .zip(snapshot_req_field_types.iter())
        .map(|(f, ty)| quote! { pub #f: #ty, })
        .collect();

    let snapshot_opt_fields: Vec<TokenStream2> = optional_field_list
        .iter()
        .zip(snapshot_opt_field_types.iter())
        .map(|(f, ty)| quote! { pub #f: #ty, })
        .collect();

    let snapshot_clone_from_req: Vec<TokenStream2> = required_field_list
        .iter()
        .map(|f| quote! { self.#f.clone_from(&other.#f); })
        .collect();

    let snapshot_clone_from_opt: Vec<TokenStream2> = optional_field_list
        .iter()
        .map(|f| {
            quote! {
                match (&mut self.#f, &other.#f) {
                    (Some(dst), Some(src)) => { dst.clone_from(src); }
                    (None, None) => {}
                    _ => { self.#f = other.#f.clone(); }
                }
            }
        })
        .collect();

    // Build snapshot assigns
    let req_rust_assigns: Vec<_> = required_field_list
        .iter()
        .zip(read_vars.iter().take(required_field_list.len()))
        .map(|(field, src_var)| {
            quote! {
                let #field = match #src_var {
                    Some(comp) => <_ as Clone>::clone(comp),
                    None => { continue; }
                };
            }
        })
        .collect();

    let opt_rust_assigns: Vec<_> = optional_field_list
        .iter()
        .zip(read_vars.iter().skip(required_field_list.len()))
        .map(|(field, src_var)| quote! { let #field = #src_var.cloned(); })
        .collect();

    // Creation DTO allocation from components via DataTransferConfig
    let snap_req_assigns: Vec<_> = required_field_list
        .iter()
        .zip(required_type_list.iter())
        .zip(read_vars.iter().take(required_field_list.len()))
        .map(|((field, cfg_ty), src_var)| {
            let f0 = format_ident!("{}0", field);
            let f1 = format_ident!("{}1", field);
            let f2 = format_ident!("{}2", field);

            quote! {
                let (#f0, #f1, #f2) = match #src_var {
                    Some(comp) => (
                        <#cfg_ty as DataTransferConfig>::from_data(comp),
                        <#cfg_ty as DataTransferConfig>::from_data(comp),
                        <#cfg_ty as DataTransferConfig>::from_data(comp),
                    ),
                    None => { continue; }
                };
            }
        })
        .collect();

    let snap_opt_assigns: Vec<_> = optional_field_list
        .iter()
        .zip(optional_type_list.iter())
        .zip(read_vars.iter().skip(required_field_list.len()))
        .map(|((field, cfg_ty), src_var)| {
            let f0 = format_ident!("{}0", field);
            let f1 = format_ident!("{}1", field);
            let f2 = format_ident!("{}2", field);

            quote! {
                let #f0 = #src_var.map(|comp| <#cfg_ty as DataTransferConfig>::from_data(comp));
                let #f1 = #src_var.map(|comp| <#cfg_ty as DataTransferConfig>::from_data(comp));
                let #f2 = #src_var.map(|comp| <#cfg_ty as DataTransferConfig>::from_data(comp));
            }
        })
        .collect();

    // In-place DTO updates via update_dto
    let update_req_in_place: Vec<_> = required_field_list
        .iter()
        .zip(required_type_list.iter())
        .zip(read_vars.iter().take(required_field_list.len()))
        .map(|((field, cfg_ty), src_var)| {
            quote! {
                match #src_var {
                    Some(comp) => {
                        let ref mut dto = d.#field;
                        <#cfg_ty as DataTransferConfig>::update_dto(dto, comp);
                    }
                    None => { continue; }
                };
            }
        })
        .collect();

    let update_opt_in_place: Vec<_> = optional_field_list
        .iter()
        .zip(optional_type_list.iter())
        .zip(read_vars.iter().skip(required_field_list.len()))
        .map(|((field, cfg_ty), src_var)| {
            quote! {
                match #src_var {
                    Some(comp) => {
                        if let Some(ref mut dto) = d.#field {
                            <#cfg_ty as DataTransferConfig>::update_dto(dto, comp);
                        } else {
                            d.#field = Some(<#cfg_ty as DataTransferConfig>::from_data(comp));
                        }
                    }
                    None => { d.#field = None; }
                };
            }
        })
        .collect();

    // Wrapper DTO field types (Gd<DtoType>)
    let wrapper_req_field_types: Vec<_> = required_dto_type_list
        .iter()
        .map(|dto| quote! { Gd<#dto> })
        .collect();
    let wrapper_opt_field_types: Vec<_> = optional_dto_type_list
        .iter()
        .map(|dto| quote! { Option<Gd<#dto>> })
        .collect();

    let wrapper_req_fields: Vec<TokenStream2> = required_field_list
        .iter()
        .zip(wrapper_req_field_types.iter())
        .map(|(f, ty)| quote! { #[var] pub #f: #ty, })
        .collect();

    let wrapper_opt_fields: Vec<TokenStream2> = optional_field_list
        .iter()
        .zip(wrapper_opt_field_types.iter())
        .map(|(f, ty)| quote! { #[var] pub #f: #ty, })
        .collect();

    // Updated queries per component
    let updated_req_idents: Vec<Ident> = (0..required_component_type_list.len())
        .map(|i| format_ident!("updated_req_{}", i))
        .collect();
    let updated_opt_idents: Vec<Ident> = (0..optional_component_type_list.len())
        .map(|i| format_ident!("updated_opt_{}", i))
        .collect();

    let mut updated_decl: Vec<TokenStream2> = Vec::new();
    for (i, comp) in required_component_type_list.iter().enumerate() {
        let id = &updated_req_idents[i];
        updated_decl
            .push(quote! { #id: Query<(Entity, &#comp), (With<#tag_ty>, Changed<#comp>)>, });
    }
    for (i, comp) in optional_component_type_list.iter().enumerate() {
        let id = &updated_opt_idents[i];
        updated_decl
            .push(quote! { #id: Query<(Entity, &#comp), (With<#tag_ty>, Changed<#comp>)>, });
    }

    let snapshot_filter = quote! { With<#tag_ty> };

    // Imports: config types + tag
    let mut import_paths: Vec<syn::Path> = Vec::new();
    for ty in required_type_list.iter().chain(optional_type_list.iter()) {
        if let Some(p) = relative_type_path(ty) {
            import_paths.push(p);
        }
    }
    if let Some(p) = relative_path(&spec.tag) {
        import_paths.push(p);
    }

    // Dedup uses
    use std::collections::HashSet;
    let mut seen: HashSet<String> = HashSet::new();
    let import_uses: Vec<TokenStream2> = import_paths
        .into_iter()
        .filter_map(|p| {
            let key = quote!(#p).to_string();
            if seen.insert(key) {
                Some(quote! { use super::#p; })
            } else {
                None
            }
        })
        .collect();

    let any_updated_terms: Vec<TokenStream2> = updated_req_idents
        .iter()
        .map(|id| quote! { !#id.is_empty() })
        .chain(
            updated_opt_idents
                .iter()
                .map(|id| quote! { !#id.is_empty() }),
        )
        .collect();

    let any_updated_expr = if any_updated_terms.is_empty() {
        quote! { false }
    } else if any_updated_terms.len() == 1 {
        let t = &any_updated_terms[0];
        quote! { #t }
    } else {
        quote! { #( #any_updated_terms )||* }
    };

    // Each config must implement DataTransferConfig, and its associated types must be sane.
    // This is what makes the macro fail early at compile time with a good error.
    let cfg_bounds_req: Vec<TokenStream2> = required_type_list
        .iter()
        .map(|cfg| {
            quote! {
                #cfg: DataTransferConfig,
                <#cfg as DataTransferConfig>::DataType: bevy::prelude::Component + Default + Clone + PartialEq,
                <#cfg as DataTransferConfig>::DtoType: godot::prelude::GodotClass + godot::obj::NewGd,
            }
        })
        .collect();

    let cfg_bounds_opt: Vec<TokenStream2> = optional_type_list
        .iter()
        .map(|cfg| {
            quote! {
                #cfg: DataTransferConfig,
                <#cfg as DataTransferConfig>::DataType: bevy::prelude::Component + Default + Clone + PartialEq,
                <#cfg as DataTransferConfig>::DtoType: godot::prelude::GodotClass + godot::obj::NewGd,
            }
        })
        .collect();

    // -------------------- EXPAND --------------------
    let expanded = quote! {
        #[allow(clippy::type_complexity, clippy::too_many_arguments)]
        mod #wrapper_module_ident {

            #( #import_uses )*

            use bevy::prelude::*;
            use bevy::ecs::system::SystemParam;
            use bevy_godot4::prelude::*;
            use bevy_godot4::godot::prelude::*;
            use ::std::collections::{HashMap, HashSet};
            use godot::classes::PackedScene;
            use godot::builtin::{NodePath, PackedInt64Array};
            use std::marker::PhantomData;

            // Snapshot (Rust) used for true-change detection
            #[derive(Clone, Debug, Default)]
            struct SnapshotRust {
                #( #snapshot_req_fields )*
                #( #snapshot_opt_fields )*
            }

            impl SnapshotRust {
                fn clone_from_in_place(&mut self, other: &Self) {
                    #( #snapshot_clone_from_req )*
                    #( #snapshot_clone_from_opt )*
                }
            }

            // Exporter accessor cache
            #[derive(SystemParam)]
            struct #exporter_accessor_ident<'w, 's> {
                gd: NonSendMut<'w, #exporter_accessor_impl_ident>,
                phantom: PhantomData<&'s ()>,
            }

            impl #exporter_accessor_ident<'_, '_> {
                fn get(&mut self, scene_tree: &mut SceneTreeRef) -> Option<Gd<#exporter_ident>> {
                    if let Some(cached) = self.gd.0.as_ref() {
                        if cached.is_instance_valid() {
                            return Some(cached.clone());
                        }
                    }

                    let Some(mut host) = scene_tree
                        .get()
                        .get_root()
                        .unwrap()
                        .get_node_or_null("BevyAppSingleton")
                    else {
                        self.gd.0 = None;
                        return None;
                    };

                    let Some(exporter) = host.try_get_node_as::<#exporter_ident>(stringify!(#exporter_ident)) else {
                        self.gd.0 = None;
                        return None;
                    };

                    self.gd.0 = Some(exporter.clone());
                    Some(exporter)
                }
            }

            #[derive(Debug, Default)]
            struct #exporter_accessor_impl_ident(Option<Gd<#exporter_ident>>);

            // Per-entity update flags
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #updates_ident {
                #( #[var] pub #all_field_list: bool, )*
                #[base] base: Base<RefCounted>,
            }

            // Wrapper DTO
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #wrapper_dto_ident {
                #( #wrapper_req_fields )*
                #( #wrapper_opt_fields )*
                #[var] pub updates: Gd<#updates_ident>,
                #[base] base: Base<RefCounted>,
            }

            impl #wrapper_dto_ident {
                fn from_snapshot(
                    #( #required_field_list: #wrapper_req_field_types, )*
                    #( #optional_field_list: #wrapper_opt_field_types, )*
                    updates: Gd<#updates_ident>,
                ) -> Gd<Self> {
                    let mut dto = Self::new_gd();
                    {
                        let mut d = dto.bind_mut();
                        #( d.#required_field_list = #required_field_list; )*
                        #( d.#optional_field_list = #optional_field_list; )*
                        d.updates = updates;
                    }
                    dto
                }
            }

            // Batch payload
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #batch_ident {
                #[var] pub created_ids: PackedInt64Array,
                #[var] pub created: Array<Gd<#wrapper_dto_ident>>,
                #[var] pub updated_ids: PackedInt64Array,
                #[var] pub updated_curr: Array<Gd<#wrapper_dto_ident>>,
                #[var] pub updated_prev: Array<Gd<#wrapper_dto_ident>>,
                #[var] pub removed_ids: PackedInt64Array,
                #[base] base: Base<RefCounted>,
            }

            // Exporter node
            #[derive(GodotClass)]
            #[class(init, base=Node)]
            pub struct #exporter_ident {
                state_cache: HashMap<i64, __EntityDtoRing>,
                state_cache_rust: HashMap<i64, SnapshotRust>,
                #[base] base: Base<Node>,
            }

            /// Per-entity DTO ring to avoid allocations and guarantee referential inequality.
            ///
            /// Ring idea:
            /// - `curr` is the current "version" emitted to Godot.
            /// - `prev` is the previous `curr` (stable reference, used for diffs/animations).
            /// - `spare` is a pre-created vacant instance we can rotate into `curr`.
            ///
            /// On update:
            /// - rotate: `prev <- curr`, `curr <- spare`, `spare <- old prev`
            /// - update `curr` in-place (including nested DTO contents) so nested references remain stable.
            #[derive(Clone, Debug)]
            struct __EntityDtoRing {
                curr: Gd<#wrapper_dto_ident>,
                prev: Gd<#wrapper_dto_ident>,
                spare: Gd<#wrapper_dto_ident>,
            }

            impl __EntityDtoRing {
                fn new(
                    curr: Gd<#wrapper_dto_ident>,
                    prev: Gd<#wrapper_dto_ident>,
                    spare: Gd<#wrapper_dto_ident>,
                ) -> Self {
                    Self { curr, prev, spare }
                }

                fn rotate(&mut self) {
                    // prev <- curr, curr <- spare, spare <- prev
                    std::mem::swap(&mut self.prev, &mut self.curr);
                    std::mem::swap(&mut self.curr, &mut self.spare);
                }
            }

            #[godot_api]
            impl #exporter_ident {
                #[signal] fn batch(batch: Gd<#batch_ident>);
            }

            // Export system
            #[allow(non_snake_case)]
            fn #system_ident(
                created: Query<Entity, #created_filter>,
                updated: Query<Entity, #updated_filter>,
                #removed_decl
                snapshot: Query<#read_tuple, #snapshot_filter>,
                #( #updated_decl )*
                mut scene_tree: SceneTreeRef,
                mut exporter_accessor: #exporter_accessor_ident,
            )
            where
                #( #cfg_bounds_req )*
                #( #cfg_bounds_opt )*
            {
                let any_created = !created.is_empty();
                let any_removed = !removed.is_empty();
                let any_updated = #any_updated_expr;
                if !any_created && !any_removed && !any_updated {
                    return;
                }

                let Some(mut exporter) = exporter_accessor.get(&mut scene_tree) else { return; };
                let mut exporter = exporter.bind_mut();

                let mut created_ids_set: HashSet<u64> = HashSet::new();

                let mut removed_ids_vec: Vec<i64> = Vec::new();
                if any_removed {
                    for entity in removed.read() {
                        removed_ids_vec.push(entity.to_bits() as i64);
                    }
                }

                // CREATED: allocate DTO rings + snapshot cache
                if any_created {
                    for entity in created.iter() {
                        created_ids_set.insert(entity.to_bits());

                        let Ok(#destructure) = snapshot.get(entity) else { continue; };

                        #( #req_rust_assigns )*
                        #( #opt_rust_assigns )*
                        let snapshot_rust = SnapshotRust {
                            #( #required_field_list: #required_field_list, )*
                            #( #optional_field_list: #optional_field_list, )*
                        };

                        #( #snap_req_assigns )*
                        #( #snap_opt_assigns )*

                        let mut updates0 = #updates_ident::new_gd();
                        { let mut u = updates0.bind_mut(); #( u.#all_field_list = true; )* }

                        let mut updates1 = #updates_ident::new_gd();
                        { let mut u = updates1.bind_mut(); #( u.#all_field_list = true; )* }

                        let mut updates2 = #updates_ident::new_gd();
                        { let mut u = updates2.bind_mut(); #( u.#all_field_list = true; )* }

                        let curr = #wrapper_dto_ident::from_snapshot(
                            #( #required_field_list0, )*
                            #( #optional_field_list0, )*
                            updates0,
                        );

                        let prev = #wrapper_dto_ident::from_snapshot(
                            #( #required_field_list1, )*
                            #( #optional_field_list1, )*
                            updates1,
                        );

                        let spare = #wrapper_dto_ident::from_snapshot(
                            #( #required_field_list2, )*
                            #( #optional_field_list2, )*
                            updates2,
                        );

                        let ring = __EntityDtoRing::new(curr, prev, spare);

                        let eid_i64 = entity.to_bits() as i64;
                        exporter.state_cache.insert(eid_i64, ring);
                        exporter.state_cache_rust.insert(eid_i64, snapshot_rust);
                    }
                }

                // UPDATED: compute field flags based on *component* equality
                use ::std::collections::HashSet as __HashSet;
                let mut changed_eids: __HashSet<u64> = __HashSet::new();

                #(
                    for (entity, _comp) in #updated_req_idents.iter() {
                        changed_eids.insert(entity.to_bits());
                    }
                )*

                #(
                    for (entity, _comp) in #updated_opt_idents.iter() {
                        changed_eids.insert(entity.to_bits());
                    }
                )*

                struct __BatchAcc {
                    created_ids: PackedInt64Array,
                    created: Array<Gd<#wrapper_dto_ident>>,
                    updated_ids: PackedInt64Array,
                    updated_curr: Array<Gd<#wrapper_dto_ident>>,
                    updated_prev: Array<Gd<#wrapper_dto_ident>>,
                    removed_ids: PackedInt64Array,
                }

                let mut acc = __BatchAcc {
                    created_ids: PackedInt64Array::new(),
                    created: Array::new(),
                    updated_ids: PackedInt64Array::new(),
                    updated_curr: Array::new(),
                    updated_prev: Array::new(),
                    removed_ids: PackedInt64Array::new(),
                };

                if any_created {
                    for eid_u64 in created_ids_set.iter().copied() {
                        let eid_i64 = eid_u64 as i64;
                        if let Some(ring) = exporter.state_cache.get(&eid_i64) {
                            acc.created_ids.push(eid_i64);
                            acc.created.push(&ring.curr);
                        }
                    }
                }

                if any_updated {
                    for eid in changed_eids.into_iter() {
                        if created_ids_set.contains(&eid) { continue; }

                        let entity = Entity::from_bits(eid);
                        let Ok(#destructure) = snapshot.get(entity) else { continue; };

                        #( #req_rust_assigns )*
                        #( #opt_rust_assigns )*
                        let snapshot_rust = SnapshotRust {
                            #( #required_field_list: #required_field_list, )*
                            #( #optional_field_list: #optional_field_list, )*
                        };

                        let eid_i64 = eid as i64;
                        let mut any_changed = false;

                        let prev_missing = !exporter.state_cache_rust.contains_key(&eid_i64);

                        let mut updates: Gd<#updates_ident>;
                        if prev_missing {
                            let Some(ring) = exporter.state_cache.get_mut(&eid_i64) else { continue; };
                            updates = ring.curr.bind().updates.clone();

                            let mut u = updates.bind_mut();
                            #( u.#all_field_list = true; )*
                            any_changed = true;
                        } else {
                            let prev_rust = exporter.state_cache_rust.get(&eid_i64).unwrap();
                            let ring = match exporter.state_cache.get(&eid_i64) {
                                Some(r) => r,
                                None => continue,
                            };
                            updates = ring.curr.bind().updates.clone();

                            let mut u = updates.bind_mut();
                            #(
                                let changed = prev_rust.#required_field_list != snapshot_rust.#required_field_list;
                                if changed { u.#required_field_list = true; any_changed = true; } else { u.#required_field_list = false; }
                            )*
                            #(
                                let changed = prev_rust.#optional_field_list != snapshot_rust.#optional_field_list;
                                if changed { u.#optional_field_list = true; any_changed = true; } else { u.#optional_field_list = false; }
                            )*
                        }

                        if !any_changed { continue; }

                        let (mut curr_for_emit, prev_for_emit): (Gd<#wrapper_dto_ident>, Gd<#wrapper_dto_ident>) = {
                            let Some(ring) = exporter.state_cache.get_mut(&eid_i64) else { continue; };
                            ring.rotate();
                            (ring.curr.clone(), ring.prev.clone())
                        };

                        {
                            let mut d = curr_for_emit.bind_mut();
                            #( #update_req_in_place )*
                            #( #update_opt_in_place )*
                        }

                        if let Some(prev) = exporter.state_cache_rust.get_mut(&eid_i64) {
                            prev.clone_from_in_place(&snapshot_rust);
                        } else {
                            exporter.state_cache_rust.insert(eid_i64, snapshot_rust);
                        }

                        acc.updated_ids.push(eid_i64);
                        acc.updated_curr.push(&curr_for_emit);
                        acc.updated_prev.push(&prev_for_emit);
                    }
                }

                if any_removed && !removed_ids_vec.is_empty() {
                    for &id_i64 in removed_ids_vec.iter() {
                        exporter.state_cache.remove(&id_i64);
                        exporter.state_cache_rust.remove(&id_i64);
                        acc.removed_ids.push(id_i64);
                    }
                }

                let any =
                    acc.created_ids.len() > 0
                    || acc.updated_ids.len() > 0
                    || acc.removed_ids.len() > 0;

                if any {
                    let mut batch = #batch_ident::new_gd();
                    {
                        let mut b = batch.bind_mut();
                        b.created_ids = acc.created_ids;
                        b.created = acc.created;
                        b.updated_ids = acc.updated_ids;
                        b.updated_curr = acc.updated_curr;
                        b.updated_prev = acc.updated_prev;
                        b.removed_ids = acc.removed_ids;
                    }
                    exporter.signals().batch().emit(&batch);
                }
            }

            pub struct #plugin_ident;
            impl Plugin for #plugin_ident {
                fn build(&self, app: &mut App) {
                    app.init_non_send_resource::<#exporter_accessor_impl_ident>();
                    app.add_systems(PostUpdate, #system_ident);
                }
            }

            // -------- Runtime Entity Node (virtual hooks, no signals) --------
            #[derive(GodotClass)]
            #[class(base = Node)]
            pub struct #entity_node_ident {
                #[var] pub entity_id: i64,
                #[var] pub last_state: Option<Gd<#wrapper_dto_ident>>,
                #[var] custom_cleanup_enabled: bool,
                #[base] base: Base<Node>,
            }

            #[godot_api]
            impl #entity_node_ident {
                #[func]
                fn set_custom_cleanup(&mut self, enabled: bool) {
                    self.custom_cleanup_enabled = enabled;
                }

                #[func(virtual)]
                fn on_updated(&mut self, curr: Gd<#wrapper_dto_ident>, _prev: Gd<#wrapper_dto_ident>) {
                    self.last_state = Some(curr);
                }

                #[func(virtual)]
                fn on_removed(&mut self) {}
            }

            #[godot_api]
            impl INode for #entity_node_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        entity_id: -1,
                        last_state: None,
                        custom_cleanup_enabled: false,
                        base,
                    }
                }
            }

            // -------- Spawn Handler --------
            #[derive(GodotClass)]
            #[class(base = Node)]
            pub struct #entity_spawn_handler_ident {
                #[var] scene: Gd<PackedScene>,
                #[var] parent_path: NodePath,
                map: HashMap<i64, Gd<#entity_node_ident>>,
                #[base] base: Base<Node>,
            }

            #[godot_api]
            impl #entity_spawn_handler_ident {
                fn parent_node(&mut self) -> Gd<Node> {
                    if self.parent_path.is_empty() {
                        self.base().clone().upcast()
                    } else {
                        self.base().get_node_as::<Node>(&self.parent_path)
                    }
                }

                #[func]
                fn _on_created(&mut self, entity_id: i64, curr: Gd<#wrapper_dto_ident>) {
                    let mut parent: Gd<Node> = self.parent_node();

                    let inst = self.scene.instantiate().unwrap();
                    parent.add_child(&inst);

                    let Ok(mut ent) = inst.clone().try_cast::<#entity_node_ident>() else {
                        godot_error!("Spawned scene must extend {}", stringify!(#entity_node_ident));
                        return;
                    };

                    ent.bind_mut().entity_id = entity_id;

                    // For first frame, prev == curr (convenient default)
                    if ent.is_instance_valid() {
                        ent.bind_mut().on_updated(curr.clone(), curr);
                    }

                    self.map.insert(entity_id, ent);
                }

                #[func]
                fn _on_updated(&mut self, entity_id: i64, curr: Gd<#wrapper_dto_ident>, prev: Gd<#wrapper_dto_ident>) {
                    if let Some(ent) = self.map.get_mut(&entity_id) {
                        if ent.is_instance_valid() {
                            ent.bind_mut().on_updated(curr, prev);
                        }
                    }
                }

                #[func]
                fn _on_removed(&mut self, entity_id: i64) {
                    if let Some(mut ent) = self.map.remove(&entity_id) {
                        if !ent.is_instance_valid() {
                            return;
                        }

                        let custom = { ent.bind().custom_cleanup_enabled };
                        if custom {
                            ent.bind_mut().on_removed();
                        } else {
                            ent.upcast::<Node>().queue_free();
                        }
                    }
                }

                #[func]
                fn _on_batch(&mut self, batch: Gd<#batch_ident>) {
                    let b = batch.bind();

                    // CREATED
                    let created_len: usize = b.created_ids.len() as usize;
                    for idx in 0..created_len {
                        let Some(entity_id) = b.created_ids.get(idx) else { continue; };
                        let Some(curr) = b.created.get(idx) else { continue; };
                        self._on_created(entity_id, curr);
                    }

                    // UPDATED
                    let updated_len: usize = b.updated_ids.len() as usize;
                    for idx in 0..updated_len {
                        let Some(entity_id) = b.updated_ids.get(idx) else { continue; };
                        let Some(curr) = b.updated_curr.get(idx) else { continue; };
                        let Some(prev) = b.updated_prev.get(idx) else { continue; };
                        self._on_updated(entity_id, curr, prev);
                    }

                    // REMOVED
                    let removed_len: usize = b.removed_ids.len() as usize;
                    for idx in 0..removed_len {
                        let Some(entity_id) = b.removed_ids.get(idx) else { continue; };
                        self._on_removed(entity_id);
                    }
                }

                #[func]
                fn get_entity_or_null(&mut self, entity_id: i64) -> Option<Gd<#entity_node_ident>> {
                    self.map.get(&entity_id).cloned()
                }
            }

            #[godot_api]
            impl INode for #entity_spawn_handler_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        scene: PackedScene::new_gd(),
                        parent_path: NodePath::default(),
                        map: HashMap::new(),
                        base,
                    }
                }

                fn ready(&mut self) {
                    let tree = self.base().get_tree().unwrap();
                    let root = tree.get_root().unwrap();
                    let Some(app) = root.try_get_node_as::<Node>("BevyAppSingleton") else {
                        godot_error!("BevyAppSingleton not found at /root");
                        return;
                    };
                    let Some(mut exporter) = app.try_get_node_as::<Node>(stringify!(#exporter_ident)) else {
                        godot_warn!("{} not found under BevyAppSingleton", stringify!(#exporter_ident));
                        return;
                    };

                    let on_batch = self.base().callable("_on_batch");
                    let _ = exporter.connect("batch", &on_batch);
                }
            }
        }

        pub use #wrapper_module_ident::{ #wrapper_dto_ident, #exporter_ident, #plugin_ident };
    };

    expanded.into()
}
