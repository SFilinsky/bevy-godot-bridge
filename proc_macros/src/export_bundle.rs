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

    // **Locals for prev/curr identifiers used in expansion**
    let req_prev_ids: Vec<Ident> = req_fields
        .iter()
        .map(|f| format_ident!("prev_{}", f))
        .collect();
    let req_curr_ids: Vec<Ident> = req_fields
        .iter()
        .map(|f| format_ident!("curr_{}", f))
        .collect();
    let opt_prev_ids: Vec<Ident> = opt_fields
        .iter()
        .map(|f| format_ident!("prev_{}", f))
        .collect();
    let opt_curr_ids: Vec<Ident> = opt_fields
        .iter()
        .map(|f| format_ident!("curr_{}", f))
        .collect();

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
                { let mut ex = exporter.bind_mut(); ex.state_cache.remove(&id_i64); }
                exporter.signals().removed().emit(id_i64);
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

    // Build wrapper assigns
    let req_assigns: Vec<_> = req_fields
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

    let opt_assigns: Vec<_> = opt_fields
        .iter()
        .zip(opt_tys.iter())
        .zip(read_vars.iter().skip(req_fields.len()))
        .map(|((field, dto_ty), src_var)| {
            quote! {
                let #field = #src_var.map(|comp| <#dto_ty as DtoFrom<<#dto_ty as DTO>::Component>>::dto_from(comp));
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

    // -------------------- EXPAND --------------------
    let expanded = quote! {
        #[allow(clippy::type_complexity, clippy::too_many_arguments)]
        mod #wrapper_module_ident {

            #( #import_uses )*

            use bevy::prelude::*;
            use bevy_godot4::prelude::*;
            use bevy_godot4::godot::prelude::*;
            use bevy_godot4::prelude::AsVisualSystem;
            use bevy_godot4::collect_children;
            use ::std::collections::{HashMap, HashSet};
            use godot::classes::PackedScene;
            use godot::builtin::NodePath;

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

            // -------- Exporter Node --------
            #[derive(GodotClass)]
            #[class(init, base=Node)]
            pub struct #exporter_ident {
                /// Previous wrapper per entity id
                state_cache: HashMap<i64, Gd<#wrapper_dto_ident>>,

                #[base]
                base: Base<Node>,
            }

            #[godot_api]
            impl #exporter_ident {
                #[signal] fn created(entity_id: i64, dto: Gd<#wrapper_dto_ident>);
                #[signal] fn updated(entity_id: i64, curr: Gd<#wrapper_dto_ident>, prev: Gd<#wrapper_dto_ident>);
                #[signal] fn removed(entity_id: i64);
            }

            // -------- System --------
            #[allow(non_snake_case)]
            fn #system_ident(
                // created
                created: Query<
                    Entity,
                    #created_filter
                >,

                // updated candidates (we'll refine via DTO equality)
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
            )
            where
                #( #dtofrom_bounds_req, )*
                #( #dtofrom_bounds_opt, )*
            {
                let any_created = !created.is_empty();
                let any_removed = !removed.is_empty();
                let any_updated = #any_updated_expr;
                if !any_created && !any_removed && !any_updated {
                    return;
                }

                let Some(mut host) = scene_tree
                    .get()
                    .get_root()
                    .unwrap()
                    .get_node_or_null("BevyAppSingleton")
                else { return; };

                let mut exporters = collect_children::<#exporter_ident>(host, true);
                if exporters.is_empty() { return; }

                let mut created_ids: HashSet<u64> = HashSet::new();

                // ---------------- CREATED ----------------
                for entity in created.iter() {
                    created_ids.insert(entity.to_bits());

                    let Ok(#destructure) = snapshot.get(entity) else { continue; };

                    #( #req_assigns )*
                    #( #opt_assigns )*

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
                        { let mut ex = exporter.bind_mut(); ex.state_cache.insert(eid_i64, wrapper.clone()); }
                        exporter.signals().created().emit(eid_i64, &wrapper);
                    }
                }

                // ---------------- UPDATED ----------------
                use ::std::collections::{HashMap as __HashMap, HashSet as __HashSet};
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

                for eid in changed_eids.into_iter() {
                    if created_ids.contains(&eid) { continue; }

                    let entity = Entity::from_bits(eid);

                    if let Ok(#destructure) = snapshot.get(entity) {
                        #( #req_assigns )*
                        #( #opt_assigns )*

                        // NOTE: curr must be mutable because we set curr.updates later
                        let mut curr = #wrapper_dto_ident::from_snapshot(
                            #( #req_fields.clone(), )*
                            #( #opt_fields.clone(), )*
                            #updates_ident::new_gd(), // will be set below
                        );

                        let eid_i64 = eid as i64;

                        for exporter in exporters.iter_mut() {
                            // Pull previous wrapper (clone Option) immutably.
                            let prev_opt: Option<Gd<#wrapper_dto_ident>> = {
                                let ex = exporter.bind();
                                ex.state_cache.get(&eid_i64).cloned()
                            };

                            // Compute updates via DTO equality, using borrows.
                            let mut updates = #updates_ident::new_gd();
                            let mut any_changed = false;

                            if prev_opt.is_none() {
                                // First-seen update under this exporter: all flags = true
                                let mut u = updates.bind_mut();
                                #( u.#all_fields = true; )*
                                any_changed = true;
                            } else if let Some(prev_ref) = prev_opt.as_ref() {
                                // Extract per-field Gd clones into locals
                                #(
                                let (#req_prev_ids, #req_curr_ids) = {
                                    let pw = prev_ref.bind();
                                    let cw = curr.bind();
                                    (pw.#req_fields.clone(), cw.#req_fields.clone())
                                };
                                )*
                                #(
                                let (#opt_prev_ids, #opt_curr_ids) = {
                                    let pw = prev_ref.bind();
                                    let cw = curr.bind();
                                    (pw.#opt_fields.clone(), cw.#opt_fields.clone())
                                };
                                )*

                                {
                                    let mut u = updates.bind_mut();

                                    // required fields equality
                                    #(
                                    let changed = {
                                        let pb = #req_prev_ids.bind();
                                        let cb = #req_curr_ids.bind();
                                        *pb != *cb
                                    };
                                    if changed { u.#req_fields = true; any_changed = true; } else { u.#req_fields = false; }
                                    )*

                                    // optional fields equality (Option<Gd<Dto>>)
                                    #(
                                    let changed = match (#opt_prev_ids, #opt_curr_ids) {
                                        (None, None) => false,
                                        (Some(p), Some(c)) => {
                                            let pb = p.bind();
                                            let cb = c.bind();
                                            *pb != *cb
                                        }
                                        _ => true,
                                    };
                                    if changed { u.#opt_fields = true; any_changed = true; } else { u.#opt_fields = false; }
                                    )*
                                }
                            }

                            if !any_changed {
                                // No actual DTO changes; skip emit and DO NOT overwrite cache.
                                continue;
                            }

                            // Attach updates to curr (requires curr to be mutable binding)
                            {
                                let mut c = curr.bind_mut();
                                c.updates = updates.clone();
                            }

                            // Prepare prev_for_emit without moving prev_opt.
                            let prev_for_emit: Gd<#wrapper_dto_ident> =
                                prev_opt.as_ref().cloned().unwrap_or_else(|| curr.clone());

                            // Emit and update cache AFTER successful compute
                            {
                                let mut ex = exporter.bind_mut();
                                ex.state_cache.insert(eid_i64, curr.clone());
                            }
                            exporter.signals().updated().emit(eid_i64, &curr, &prev_for_emit);
                        }
                    }
                }

                // ---------------- REMOVED ----------------
                #removed_loop
            }

            // -------- Plugin --------
            pub struct #plugin_ident;
            impl Plugin for #plugin_ident {
                fn build(&self, app: &mut App) {
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

                    let on_created = self.base().callable("_on_created");
                    let on_updated = self.base().callable("_on_updated");
                    let on_removed = self.base().callable("_on_removed");

                    let _ = exporter.connect("created", &on_created);
                    // updated(curr, prev)
                    let _ = exporter.connect("updated", &on_updated);
                    let _ = exporter.connect("removed", &on_removed);
                }
            }

        }

        pub use #wrapper_module_ident::{ #wrapper_dto_ident, #exporter_ident, #plugin_ident };
    };

    expanded.into()
}
