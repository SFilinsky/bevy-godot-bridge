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
///   dtos: [ TransformDto, HealthDto, Option<MovementDto> ],
/// }
struct Spec {
    name: LitStr,
    tag: Path,
    items: Vec<DtoItem>,
}

enum DtoItem {
    Required(Type), // FooDto
    Optional(Type), // Option<FooDto>
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name: Option<LitStr> = None;
        let mut tag: Option<Path> = None;
        let mut items: Option<Vec<DtoItem>> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "name" => {
                    name = Some(input.parse()?);
                }
                "tag" => {
                    tag = Some(input.parse()?);
                }
                "dtos" => {
                    let list;
                    bracketed!(list in input);

                    let mut v: Vec<DtoItem> = Vec::new();
                    while !list.is_empty() {
                        let ty: Type = list.parse()?;
                        let ts = quote!(#ty).to_string();
                        if ts.starts_with("Option <") || ts.starts_with("Option<") {
                            v.push(DtoItem::Optional(ty));
                        } else {
                            v.push(DtoItem::Required(ty));
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
                        format!("unknown key `{other}`; expected `name`, `tag`, or `dtos`"),
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
            items.ok_or_else(|| syn::Error::new(input.span(), "missing `dtos: [ ... ]`"))?;

        Ok(Spec { name, tag, items })
    }
}

fn dto_field_ident(ty: &Type) -> Ident {
    fn last_seg_name(ty: &Type) -> String {
        match ty {
            Type::Path(tp) => tp
                .path
                .segments
                .last()
                .map(|s| s.ident.to_string())
                .unwrap_or_else(|| "dto".to_string()),
            Type::Paren(p) => last_seg_name(&p.elem),
            _ => "dto".to_string(),
        }
    }

    // Strip Option<...> for field naming if present
    let raw = quote!(#ty).to_string();
    let base = if raw.starts_with("Option <") || raw.starts_with("Option<") {
        let cut = raw.trim();
        let inner = &cut[cut.find('<').unwrap() + 1..cut.rfind('>').unwrap()];
        inner.trim().split("::").last().unwrap_or(inner).to_string()
    } else {
        last_seg_name(ty)
    };

    let base = base.strip_suffix("Dto").unwrap_or(&base).to_string();
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

    // Split items into required / optional DTO types
    let mut req_tys: Vec<Type> = Vec::new();
    let mut opt_tys: Vec<Type> = Vec::new();

    for it in spec.items.iter() {
        match it {
            DtoItem::Required(ty) => req_tys.push(strip_option(ty)),
            DtoItem::Optional(ty) => opt_tys.push(strip_option(ty)),
        }
    }

    // Field idents for each DTO
    let req_fields: Vec<Ident> = req_tys.iter().map(dto_field_ident).collect();
    let opt_fields: Vec<Ident> = opt_tys.iter().map(dto_field_ident).collect();

    // Component types for each DTO via <Dto as DTO>::Component
    let req_comps: Vec<TokenStream2> = req_tys
        .iter()
        .map(|t| quote! { <#t as DTO>::Component })
        .collect();
    let opt_comps: Vec<TokenStream2> = opt_tys
        .iter()
        .map(|t| quote! { <#t as DTO>::Component })
        .collect();

    // All fields/types (req then opt)
    let all_fields: Vec<Ident> = req_fields
        .iter()
        .cloned()
        .chain(opt_fields.iter().cloned())
        .collect();

    let all_tys: Vec<Type> = req_tys
        .iter()
        .cloned()
        .chain(opt_tys.iter().cloned())
        .collect();

    // Bounds for DtoFrom on all DTOs — require PartialEq on DTO (inner type)
    let dtofrom_bounds_req: Vec<TokenStream2> = req_tys
        .iter()
        .zip(req_comps.iter())
        .map(|(dto, comp)| quote! { #dto: DTO + DtoFrom<#comp> + PartialEq })
        .collect();

    let dtofrom_bounds_opt: Vec<TokenStream2> = opt_tys
        .iter()
        .zip(opt_comps.iter())
        .map(|(dto, comp)| quote! { #dto: DTO + DtoFrom<#comp> + PartialEq })
        .collect();

    // Bounds for component cloning into SnapshotRust
    let comp_bounds_req: Vec<TokenStream2> = req_comps
        .iter()
        .map(|c| quote! { #c: Clone + PartialEq })
        .collect();

    let comp_bounds_opt: Vec<TokenStream2> = opt_comps
        .iter()
        .map(|c| quote! { #c: Clone + PartialEq })
        .collect();

    // --- Filters -------------------------------------------------------------
    let tag_ty = &spec.tag;

    // CREATED filter: tag added
    let created_filter = quote! { Added<#tag_ty> };

    // UPDATED filter – still use Changed to reduce the candidate set
    let changed_terms: Vec<_> = req_comps
        .iter()
        .chain(opt_comps.iter())
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

    // REMOVED decl + loop
    let removed_decl = quote! { mut removed: RemovedComponents<#tag_ty>, };

    let removed_loop = quote! {
        for entity in removed.read() {
            let id_i64 = entity.to_bits() as i64;
            for exporter in exporters.iter_mut() {
                {
                    let mut ex = exporter.bind_mut();
                    ex.state_cache.remove(&id_i64);
                    ex.state_cache_rust.remove(&id_i64);
                }
                // batched: removed will be emitted via batch
            }
        }
    };

    // Snapshot tuple and vars (Option<&Ci> for all Ci; req then opt)
    let read_types: Vec<_> = req_comps
        .iter()
        .map(|c| quote! { Option<& #c> })
        .chain(opt_comps.iter().map(|c| quote! { Option<& #c> }))
        .collect();

    let read_tuple: TokenStream2 = if read_types.is_empty() {
        quote! { () }
    } else {
        quote! {
            (
                #(
                    #read_types,
                )*
            )
        }
    };

    let read_vars: Vec<Ident> = all_fields
        .iter()
        .map(|f| format_ident!("__{}", f))
        .collect();

    let destructure = if read_vars.is_empty() {
        quote! { () }
    } else {
        quote! {
            (
                #(
                    #read_vars,
                )*
            )
        }
    };

    // Build wrapper assigns (Godot DTO creation)
    let snap_req_assigns: Vec<_> = req_fields
        .iter()
        .zip(req_tys.iter())
        .zip(read_vars.iter().take(req_fields.len()))
        .map(|((field, dto_ty), src_var)| {
            quote! {
                let #field = match #src_var {
                    Some(comp) => <#dto_ty as DtoFrom<<#dto_ty as DTO>::Component>>::dto_from(comp),
                    None => { continue; } // required missing -> skip emit
                };
            }
        })
        .collect();

    let snap_opt_assigns: Vec<_> = opt_fields
        .iter()
        .zip(opt_tys.iter())
        .zip(read_vars.iter().skip(req_fields.len()))
        .map(|((field, dto_ty), src_var)| {
            quote! {
                let #field = #src_var.map(|comp| <#dto_ty as DtoFrom<<#dto_ty as DTO>::Component>>::dto_from(comp));
            }
        })
        .collect();

    // Build Rust snapshot assigns (cloning components, but updating cache uses clone_from later)
    let req_rust_assigns: Vec<_> = req_fields
        .iter()
        .zip(req_comps.iter())
        .zip(read_vars.iter().take(req_fields.len()))
        .map(|((field, comp_ty), src_var)| {
            quote! {
                let #field = match #src_var {
                    Some(comp) => <#comp_ty as Clone>::clone(comp),
                    None => { continue; }
                };
            }
        })
        .collect();

    let opt_rust_assigns: Vec<_> = opt_fields
        .iter()
        .zip(opt_comps.iter())
        .zip(read_vars.iter().skip(req_fields.len()))
        .map(|((field, _comp_ty), src_var)| {
            quote! {
                let #field = #src_var.cloned();
            }
        })
        .collect();

    // Wrapper DTO fields (zip)
    let wrapper_req_field_types: Vec<_> = req_tys
        .iter()
        .map(|dto_ty| quote! { Gd<#dto_ty> })
        .collect();
    let wrapper_opt_field_types: Vec<_> = opt_tys
        .iter()
        .map(|dto_ty| quote! { Option<Gd<#dto_ty>> })
        .collect();

    let wrapper_req_fields: Vec<TokenStream2> = req_fields
        .iter()
        .zip(wrapper_req_field_types.iter())
        .map(|(f, ty)| quote! { #[var] pub #f: #ty, })
        .collect();

    let wrapper_opt_fields: Vec<TokenStream2> = opt_fields
        .iter()
        .zip(wrapper_opt_field_types.iter())
        .map(|(f, ty)| quote! { #[var] pub #f: #ty, })
        .collect();

    // Per-DTO update queries (idents)
    let updated_req_idents: Vec<Ident> = (0..req_tys.len())
        .map(|i| format_ident!("updated_req_{}", i))
        .collect();
    let updated_opt_idents: Vec<Ident> = (0..opt_tys.len())
        .map(|j| format_ident!("updated_opt_{}", j))
        .collect();

    // Build updated query decls (always With<tag>)
    let mut updated_decl: Vec<TokenStream2> = Vec::new();
    for (i, comp) in req_comps.iter().enumerate() {
        let id = &updated_req_idents[i];
        updated_decl
            .push(quote! { #id: Query<(Entity, &#comp), (With<#tag_ty>, Changed<#comp>)>, });
    }
    for (j, comp) in opt_comps.iter().enumerate() {
        let id = &updated_opt_idents[j];
        updated_decl
            .push(quote! { #id: Query<(Entity, &#comp), (With<#tag_ty>, Changed<#comp>)>, });
    }

    // Snapshot filter (always With<tag>)
    let snapshot_filter = quote! { With<#tag_ty> };

    // DTO paths (req + opt)
    let mut import_paths: Vec<syn::Path> = Vec::new();
    for ty in all_tys.iter() {
        if let Some(p) = relative_type_path(ty) {
            import_paths.push(p);
        }
    }
    // Tag path
    if let Some(p) = relative_path(&spec.tag) {
        import_paths.push(p);
    }

    // Dedup by stringified path
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

    // SnapshotRust field types
    let snapshot_req_field_types: Vec<TokenStream2> =
        req_comps.iter().map(|c| quote! { #c }).collect();
    let snapshot_opt_field_types: Vec<TokenStream2> =
        opt_comps.iter().map(|c| quote! { Option<#c> }).collect();

    let snapshot_req_fields: Vec<TokenStream2> = req_fields
        .iter()
        .zip(snapshot_req_field_types.iter())
        .map(|(f, ty)| quote! { pub #f: #ty, })
        .collect();

    let snapshot_opt_fields: Vec<TokenStream2> = opt_fields
        .iter()
        .zip(snapshot_opt_field_types.iter())
        .map(|(f, ty)| quote! { pub #f: #ty, })
        .collect();

    // SnapshotRust clone_from per field
    let snapshot_clone_from_req: Vec<TokenStream2> = req_fields
        .iter()
        .map(|f| quote! { self.#f.clone_from(&other.#f); })
        .collect();

    let snapshot_clone_from_opt: Vec<TokenStream2> = opt_fields
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

    // -------------------- EXPAND --------------------
    let expanded = quote! {
        #[allow(clippy::type_complexity, clippy::too_many_arguments)]
        mod #wrapper_module_ident {

            #( #import_uses )*

            use bevy::prelude::*;
            use bevy::ecs::system::SystemParam;
            use bevy_godot4::prelude::*;
            use bevy_godot4::godot::prelude::*;
            use bevy_godot4::prelude::AsVisualSystem;
            use bevy_godot4::collect_children;
            use ::std::collections::{HashMap, HashSet};
            use godot::classes::PackedScene;
            use godot::builtin::{NodePath, PackedInt64Array};
            use std::marker::PhantomData;

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

            #[derive(SystemParam)]
            struct #exporter_accessor_ident<'w, 's> {
                gd: NonSendMut<'w, #exporter_accessor_impl_ident>,
                phantom: PhantomData<&'s ()>,
            }

            impl #exporter_accessor_ident<'_, '_> {
                fn get(&mut self, scene_tree: &mut SceneTreeRef) -> Vec<Gd<#exporter_ident>> {
                    if let Some(cached) = self.gd.0.as_ref() {
                        let mut out: Vec<Gd<#exporter_ident>> = Vec::new();
                        for e in cached.iter() {
                            if e.is_instance_valid() {
                                out.push(e.clone());
                            }
                        }
                        if !out.is_empty() {
                            return out;
                        }
                    }

                    let Some(mut host) = scene_tree
                        .get()
                        .get_root()
                        .unwrap()
                        .get_node_or_null("BevyAppSingleton")
                    else {
                        self.gd.0 = Some(Vec::new());
                        return Vec::new();
                    };

                    let exporters = collect_children::<#exporter_ident>(host, true);
                    self.gd.0 = Some(exporters.clone());
                    exporters
                }
            }

            #[derive(Debug, Default)]
            struct #exporter_accessor_impl_ident(Option<Vec<Gd<#exporter_ident>>>);

            // -------- Per-entity typed UpdateInfo (bool flags) --------
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #updates_ident {
                #( #[var] pub #all_fields: bool, )*
                #[base] base: Base<RefCounted>,
            }

            // -------- Wrapper DTO --------
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #wrapper_dto_ident {
                #( #wrapper_req_fields )*
                #( #wrapper_opt_fields )*

                /// Typed updates struct
                #[var]
                pub updates: Gd<#updates_ident>,

                #[base] base: Base<RefCounted>,
            }

            impl #wrapper_dto_ident {
                fn from_snapshot(
                    #( #req_fields: Gd<#req_tys>, )*
                    #( #opt_fields: Option<Gd<#opt_tys>>, )*
                    updates: Gd<#updates_ident>,
                ) -> Gd<Self> {
                    let mut dto = Self::new_gd();
                    {
                        let mut d = dto.bind_mut();
                        #( d.#req_fields = #req_fields; )*
                        #( d.#opt_fields = #opt_fields; )*
                        d.updates = updates;
                    }
                    dto
                }
            }

            // -------- Batch payload (typed, no Variant container) --------
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

            // -------- Exporter Node --------
            #[derive(GodotClass)]
            #[class(init, base=Node)]
            pub struct #exporter_ident {
                /// Previous wrapper per entity id
                state_cache: HashMap<i64, Gd<#wrapper_dto_ident>>,

                /// Previous Rust snapshot per entity id (for fast compare)
                state_cache_rust: HashMap<i64, SnapshotRust>,

                #[base]
                base: Base<Node>,
            }

            #[godot_api]
            impl #exporter_ident {
                #[signal] fn batch(batch: Gd<#batch_ident>);
            }

            // -------- System --------
            #[allow(non_snake_case)]
            fn #system_ident(
                // created
                created: Query<
                    Entity,
                    #created_filter
                >,

                // updated candidates (we'll refine via Rust snapshot equality)
                updated: Query<
                    Entity,
                    #updated_filter
                >,

                // removed
                #removed_decl

                // snapshot of current components on entity
                snapshot: Query<
                    #read_tuple,
                    #snapshot_filter
                >,

                // per-component 'Changed' queries (used to build candidate set)
                #( #updated_decl )*

                mut scene_tree: SceneTreeRef,
                mut exporter_accessor: #exporter_accessor_ident,
            )
            where
                #( #dtofrom_bounds_req, )*
                #( #dtofrom_bounds_opt, )*
                #( #comp_bounds_req, )*
                #( #comp_bounds_opt, )*
            {
                let any_created = !created.is_empty();
                let any_removed = !removed.is_empty();
                let any_updated = #any_updated_expr;
                if !any_created && !any_removed && !any_updated {
                    return;
                }

                let mut exporters = exporter_accessor.get(&mut scene_tree);
                if exporters.is_empty() { return; }

                let mut created_ids_set: HashSet<u64> = HashSet::new();

                // We collect removed ids once; per-exporter we'll emit them in batch.
                let mut removed_ids_vec: Vec<i64> = Vec::new();
                if any_removed {
                    for entity in removed.read() {
                        removed_ids_vec.push(entity.to_bits() as i64);
                    }
                }

                // ---------------- CREATED ----------------
                // For CREATED we currently build per-exporter anyway because caches are per-exporter.
                // We'll also batch per-exporter below.
                if any_created {
                    for entity in created.iter() {
                        created_ids_set.insert(entity.to_bits());

                        let Ok(#destructure) = snapshot.get(entity) else { continue; };

                        // Build Rust snapshot once
                        #( #req_rust_assigns )*
                        #( #opt_rust_assigns )*
                        let snapshot_rust = SnapshotRust {
                            #( #req_fields: #req_fields.clone(), )*
                            #( #opt_fields: #opt_fields.clone(), )*
                        };

                        // Godot DTO creation as before
                        #( #snap_req_assigns )*
                        #( #snap_opt_assigns )*

                        // updates: everything true on creation
                        let mut updates = #updates_ident::new_gd();
                        {
                            let mut u = updates.bind_mut();
                            #( u.#all_fields = true; )*
                        }

                        let wrapper = #wrapper_dto_ident::from_snapshot(
                            #( #req_fields, )*
                            #( #opt_fields, )*
                            updates,
                        );

                        let eid_i64 = entity.to_bits() as i64;

                        for exporter in exporters.iter_mut() {
                            {
                                let mut ex = exporter.bind_mut();
                                ex.state_cache.insert(eid_i64, wrapper.clone());
                                ex.state_cache_rust.insert(eid_i64, snapshot_rust.clone());
                            }
                        }
                    }
                }

                // ---------------- UPDATED ----------------
                use ::std::collections::{HashSet as __HashSet};
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

                // Per-exporter batching containers
                struct __BatchAcc {
                    created_ids: PackedInt64Array,
                    created: Array<Gd<#wrapper_dto_ident>>,
                    updated_ids: PackedInt64Array,
                    updated_curr: Array<Gd<#wrapper_dto_ident>>,
                    updated_prev: Array<Gd<#wrapper_dto_ident>>,
                    removed_ids: PackedInt64Array,
                }

                let mut batches: Vec<__BatchAcc> = Vec::with_capacity(exporters.len());
                for _ in 0..exporters.len() {
                    batches.push(__BatchAcc {
                        created_ids: PackedInt64Array::new(),
                        created: Array::new(),
                        updated_ids: PackedInt64Array::new(),
                        updated_curr: Array::new(),
                        updated_prev: Array::new(),
                        removed_ids: PackedInt64Array::new(),
                    });
                }

                // Fill CREATED batches from caches (cheap: just ids + cached wrapper clones)
                if any_created {
                    for (idx, exporter) in exporters.iter().enumerate() {
                        let mut acc = &mut batches[idx];
                        let ex = exporter.bind();

                        for eid_u64 in created_ids_set.iter().copied() {
                            let eid_i64 = eid_u64 as i64;
                            if let Some(dto) = ex.state_cache.get(&eid_i64) {
                                acc.created_ids.push(eid_i64);
                                acc.created.push(dto); // push &Gd<T>
                            }
                        }
                    }
                }

                // UPDATED: compute per entity, then per exporter decide if changed, and push to that exporter's batch.
                if any_updated {
                    for eid in changed_eids.into_iter() {
                        if created_ids_set.contains(&eid) { continue; }

                        let entity = Entity::from_bits(eid);

                        if let Ok(#destructure) = snapshot.get(entity) {
                            // Build Rust snapshot once per entity
                            #( #req_rust_assigns )*
                            #( #opt_rust_assigns )*
                            let snapshot_rust = SnapshotRust {
                                #( #req_fields: #req_fields, )*
                                #( #opt_fields: #opt_fields, )*
                            };

                            let eid_i64 = eid as i64;

                            for (idx, exporter) in exporters.iter_mut().enumerate() {
                                // Compute updates using Rust snapshot compare
                                let mut updates = #updates_ident::new_gd();
                                let mut any_changed = false;

                                let prev_missing = {
                                    let ex = exporter.bind();
                                    !ex.state_cache_rust.contains_key(&eid_i64)
                                };

                                if prev_missing {
                                    // First-seen update under this exporter: all flags = true
                                    let mut u = updates.bind_mut();
                                    #( u.#all_fields = true; )*
                                    any_changed = true;
                                } else {
                                    let ex = exporter.bind();
                                    let prev_rust = ex.state_cache_rust.get(&eid_i64).unwrap();

                                    {
                                        let mut u = updates.bind_mut();

                                        #(
                                        let changed = prev_rust.#req_fields != snapshot_rust.#req_fields;
                                        if changed { u.#req_fields = true; any_changed = true; } else { u.#req_fields = false; }
                                        )*

                                        #(
                                        let changed = prev_rust.#opt_fields != snapshot_rust.#opt_fields;
                                        if changed { u.#opt_fields = true; any_changed = true; } else { u.#opt_fields = false; }
                                        )*
                                    }
                                }

                                if !any_changed {
                                    continue;
                                }

                                // Build Godot DTOs only if something changed for this exporter
                                #( #snap_req_assigns )*
                                #( #snap_opt_assigns )*

                                let mut curr = #wrapper_dto_ident::from_snapshot(
                                    #( #req_fields.clone(), )*
                                    #( #opt_fields.clone(), )*
                                    #updates_ident::new_gd(), // will be set below
                                );

                                {
                                    let mut c = curr.bind_mut();
                                    c.updates = updates.clone();
                                }

                                let prev_for_emit: Gd<#wrapper_dto_ident> = {
                                    let ex = exporter.bind();
                                    ex.state_cache
                                        .get(&eid_i64)
                                        .cloned()
                                        .unwrap_or_else(|| curr.clone())
                                };

                                {
                                    let mut ex = exporter.bind_mut();

                                    ex.state_cache.insert(eid_i64, curr.clone());

                                    if let Some(prev) = ex.state_cache_rust.get_mut(&eid_i64) {
                                        prev.clone_from_in_place(&snapshot_rust);
                                    } else {
                                        ex.state_cache_rust.insert(eid_i64, snapshot_rust.clone());
                                    }
                                }

                                // push into this exporter's batch (IMPORTANT: push by reference)
                                let mut acc = &mut batches[idx];
                                acc.updated_ids.push(eid_i64);
                                acc.updated_curr.push(&curr);
                                acc.updated_prev.push(&prev_for_emit);
                            }
                        }
                    }
                }

                // REMOVED: apply removals to caches and add ids to each exporter's batch
                if any_removed && !removed_ids_vec.is_empty() {
                    for (idx, exporter) in exporters.iter_mut().enumerate() {
                        {
                            let mut ex = exporter.bind_mut();
                            for &id_i64 in removed_ids_vec.iter() {
                                ex.state_cache.remove(&id_i64);
                                ex.state_cache_rust.remove(&id_i64);
                            }
                        }

                        let mut acc = &mut batches[idx];
                        for &id_i64 in removed_ids_vec.iter() {
                            acc.removed_ids.push(id_i64);
                        }
                    }
                }

                // Emit batch per exporter (only if something is non-empty)
                for (idx, exporter) in exporters.iter_mut().enumerate() {
                    let acc = &mut batches[idx];

                    let any =
                        acc.created_ids.len() > 0
                        || acc.updated_ids.len() > 0
                        || acc.removed_ids.len() > 0;

                    if !any {
                        continue;
                    }

                    let mut batch = #batch_ident::new_gd();
                    {
                        let mut b = batch.bind_mut();
                        b.created_ids = acc.created_ids.clone();
                        b.created = acc.created.clone();

                        b.updated_ids = acc.updated_ids.clone();
                        b.updated_curr = acc.updated_curr.clone();
                        b.updated_prev = acc.updated_prev.clone();

                        b.removed_ids = acc.removed_ids.clone();
                    }

                    exporter.signals().batch().emit(&batch);
                }

                // keep old removed_loop for structure, but it's now a no-op emitter-wise
                #removed_loop
            }

            // -------- Plugin --------
            pub struct #plugin_ident;
            impl Plugin for #plugin_ident {
                fn build(&self, app: &mut App) {
                    app.init_non_send_resource::<#exporter_accessor_impl_ident>();
                    app.add_systems(PostUpdate, #system_ident);
                }
            }

            // -------- Runtime Entity Node (one per in-game entity) --------
            #[derive(GodotClass)]
            #[class(base = Node)]
            pub struct #entity_node_ident {
                /// Unique ECS entity id from Bevy
                #[var]
                pub entity_id: i64,

                #[var]
                pub last_state: Option<Gd<#wrapper_dto_ident>>,

                /// When true, removal will NOT free this node; instead we emit `on_remove`.
                /// Toggle via `set_custom_cleanup(true)`.
                #[var]
                custom_cleanup_enabled: bool,

                #[base]
                base: Base<Node>,
            }

            #[godot_api]
            impl #entity_node_ident {
                /// Apply a full wrapper DTO (you can also call this from your scripts)
                #[func]
                fn apply_dto(&mut self, dto: Gd<#wrapper_dto_ident>, prev: Gd<#wrapper_dto_ident>) {
                    self.signals().on_update().emit(&dto, &prev);
                    self.last_state = Some(dto);
                }

                /// Allow the game to opt into custom cleanup on remove.
                #[func]
                fn set_custom_cleanup(&mut self, enabled: bool) {
                    self.custom_cleanup_enabled = enabled;
                }

                /// Raised when a new wrapper is available.
                /// Signature: on_update(curr, prev)
                #[signal]
                fn on_update(curr: Gd<#wrapper_dto_ident>, prev: Gd<#wrapper_dto_ident>);

                /// Emitted when the Bevy entity despawns and we opted into custom cleanup.
                #[signal]
                fn on_remove();
            }

            #[godot_api]
            impl INode for #entity_node_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        entity_id: -1,
                        last_state: None,
                        custom_cleanup_enabled: false,
                        base
                    }
                }
            }

            // -------- Spawn Handler (binds exporter signals & manages spawned scenes) --------
            #[derive(GodotClass)]
            #[class(base = Node)]
            pub struct #entity_spawn_handler_ident {
                /// Scene that must extend `#entity_node_ident`
                #[var]
                scene: Gd<PackedScene>,

                /// Optional parent; empty = this Node
                #[var]
                parent_path: NodePath,

                /// Cache of spawned nodes
                map: HashMap<i64, Gd<#entity_node_ident>>,

                #[base]
                base: Base<Node>,
            }

            #[godot_api]
            impl #entity_spawn_handler_ident {
                #[func]
                fn _on_created(&mut self, entity_id: i64, curr: Gd<#wrapper_dto_ident>) {
                    let mut parent: Gd<Node> = if self.parent_path.is_empty() {
                        self.base().clone().upcast()
                    } else {
                        self.base().get_node_as::<Node>(&self.parent_path)
                    };

                    let inst = self.scene.instantiate().unwrap();
                    parent.add_child(&inst);

                    let Ok(mut ent) = inst.clone().try_cast::<#entity_node_ident>() else {
                        godot_error!("Spawned scene must extend {}", stringify!(#entity_node_ident));
                        return;
                    };

                    ent.bind_mut().entity_id = entity_id;

                    // For first frame, prev == curr (convenient default)
                    if ent.is_instance_valid() {
                        ent.bind_mut().apply_dto(curr.clone(), curr);
                    }

                    self.map.insert(entity_id, ent);
                }

                #[func]
                fn _on_updated(&mut self, entity_id: i64, curr: Gd<#wrapper_dto_ident>, prev: Gd<#wrapper_dto_ident>) {
                    if let Some(ent) = self.map.get_mut(&entity_id) {
                        if ent.is_instance_valid() {
                            ent.bind_mut().apply_dto(curr, prev);
                        }
                    }
                }

                #[func]
                fn _on_removed(&mut self, entity_id: i64) {
                    if let Some(mut ent) = self.map.remove(&entity_id) {
                        if ent.is_instance_valid() {
                            let custom = { ent.bind().custom_cleanup_enabled };
                            if custom {
                                ent.bind_mut().signals().on_remove().emit();
                            } else {
                                ent.upcast::<Node>().queue_free();
                            }
                        }
                    }
                }

                #[func]
                fn _on_batch(&mut self, batch: Gd<#batch_ident>) {
                    let b = batch.bind();

                    // CREATED (ids and dtos aligned by index)
                    let created_len = b.created_ids.len();
                    for i in 0..created_len {
                        let idx = i as usize;
                        let id = b.created_ids.get(i);
                        let dto = b.created.get(idx);
                        if let (Some(entity_id), Some(curr)) = (id, dto) {
                            self._on_created(entity_id, curr);
                        }
                    }

                    // UPDATED (ids, curr, prev aligned by index)
                    let updated_len = b.updated_ids.len();
                    for i in 0..updated_len {
                        let idx = i as usize;
                        let id = b.updated_ids.get(i);
                        let curr = b.updated_curr.get(idx);
                        let prev = b.updated_prev.get(idx);
                        if let (Some(entity_id), Some(curr), Some(prev)) = (id, curr, prev) {
                            self._on_updated(entity_id, curr, prev);
                        }
                    }

                    // REMOVED
                    let removed_len = b.removed_ids.len();
                    for i in 0..removed_len {
                        if let Some(entity_id) = b.removed_ids.get(i) {
                            self._on_removed(entity_id);
                        }
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
