use heck::{ToSnakeCase, ToUpperCamelCase};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use std::collections::HashSet;
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitStr, Path, Result, Token, Type,
};

#[derive(Clone)]
struct ComponentSpec {
    cfg_ty: Type,
    binding_ident: Ident,
    state_alias_ident: Ident,
    state_cache_ident: Ident,
    state_resolve_fn_ident: Ident,
}

struct Spec {
    tag: Path,
    module_ident: Ident,
    exporter_ident: Ident,
    plugin_ident: Ident,
    system_ident: Ident,
    exporter_accessor_ident: Ident,
    exporter_accessor_impl_ident: Ident,
    entity_meta_ident: Ident,
    required: Vec<ComponentSpec>,
    optional: Vec<ComponentSpec>,
    all: Vec<ComponentSpec>,
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name_lit: Option<LitStr> = None;
        let mut tag: Option<Path> = None;
        let mut items: Option<Vec<Type>> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "name" => name_lit = Some(input.parse()?),
                "tag" => tag = Some(input.parse()?),
                "components" => {
                    let list;
                    bracketed!(list in input);

                    let mut out: Vec<Type> = Vec::new();
                    while !list.is_empty() {
                        out.push(list.parse::<Type>()?);
                        if list.peek(Token![,]) {
                            list.parse::<Token![,]>()?;
                        } else {
                            break;
                        }
                    }
                    items = Some(out);
                }
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("unknown key `{other}`; expected `name`, `tag`, `components`"),
                    ));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        let name_lit = name_lit
            .ok_or_else(|| syn::Error::new(Span::call_site(), "missing `name: \"...\"`"))?;
        let tag =
            tag.ok_or_else(|| syn::Error::new(Span::call_site(), "missing `tag: SomeTag`"))?;
        let items = items
            .ok_or_else(|| syn::Error::new(Span::call_site(), "missing `components: [ ... ]`"))?;

        if items.is_empty() {
            return Err(syn::Error::new(
                Span::call_site(),
                "`components` must contain at least one transfer config",
            ));
        }

        let name = name_lit.value();
        let name_camel = name.to_upper_camel_case();
        let name_snake = name.to_snake_case();

        let module_ident = format_ident!("{}_composed_export", name_snake);
        let exporter_ident = format_ident!("{}EntityExporter", name_camel);
        let plugin_ident = format_ident!("{}EntityExportPlugin", name_camel);
        let system_ident = format_ident!("export_{}_entity_changes", name_snake);
        let exporter_accessor_ident = format_ident!("{}ExporterAccessor", name_camel);
        let exporter_accessor_impl_ident = format_ident!("{}ExporterAccessorImpl", name_camel);
        let entity_meta_ident = format_ident!("{}EntityMeta", name_camel);

        let mut required: Vec<ComponentSpec> = Vec::new();
        let mut optional: Vec<ComponentSpec> = Vec::new();
        let mut all: Vec<ComponentSpec> = Vec::new();
        let mut seen_fields: HashSet<String> = HashSet::new();

        for item in items {
            let is_optional = is_option_type(&item);
            let cfg_ty = strip_option(&item);
            let field_ident = dto_field_ident_from_cfg(&cfg_ty);

            let field_key = field_ident.to_string();
            if !seen_fields.insert(field_key.clone()) {
                return Err(syn::Error::new(
                    field_ident.span(),
                    format!("duplicate component field name `{field_key}` in `components`"),
                ));
            }

            let binding_ident = format_ident!("{}_data", field_ident);
            let state_alias_ident =
                format_ident!("{}{}StateNode", name_camel, field_key.to_upper_camel_case());
            let state_cache_ident = format_ident!("{}_state_cache", field_ident);
            let state_resolve_fn_ident = format_ident!("resolve_{}_state_node", field_ident);

            let component = ComponentSpec {
                cfg_ty,
                binding_ident,
                state_alias_ident,
                state_cache_ident,
                state_resolve_fn_ident,
            };

            if is_optional {
                optional.push(component.clone());
            } else {
                required.push(component.clone());
            }

            all.push(component);
        }

        Ok(Self {
            tag,
            module_ident,
            exporter_ident,
            plugin_ident,
            system_ident,
            exporter_accessor_ident,
            exporter_accessor_impl_ident,
            entity_meta_ident,
            required,
            optional,
            all,
        })
    }
}

fn is_option_type(ty: &Type) -> bool {
    if let Type::Path(tp) = ty {
        if let Some(seg) = tp.path.segments.last() {
            return seg.ident == "Option";
        }
    }

    false
}

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

fn dto_field_ident_from_cfg(cfg_ty: &Type) -> Ident {
    let raw = quote!(#cfg_ty).to_string();

    let base = raw
        .trim()
        .split("::")
        .last()
        .unwrap_or(raw.trim())
        .to_string();

    let base = base
        .strip_suffix("TransferConfig")
        .unwrap_or(&base)
        .to_string();

    let base = base
        .strip_suffix("ExportConfig")
        .unwrap_or(&base)
        .to_string();

    format_ident!("{}", base.to_snake_case())
}

pub fn expand(input: TokenStream) -> TokenStream {
    let spec = parse_macro_input!(input as Spec);

    let tag_ty = &spec.tag;
    let module_ident = &spec.module_ident;
    let exporter_ident = &spec.exporter_ident;
    let plugin_ident = &spec.plugin_ident;
    let system_ident = &spec.system_ident;
    let exporter_accessor_ident = &spec.exporter_accessor_ident;
    let exporter_accessor_impl_ident = &spec.exporter_accessor_impl_ident;
    let entity_meta_ident = &spec.entity_meta_ident;

    let req_cfg_tys: Vec<Type> = spec.required.iter().map(|c| c.cfg_ty.clone()).collect();
    let opt_cfg_tys: Vec<Type> = spec.optional.iter().map(|c| c.cfg_ty.clone()).collect();
    let all_cfg_tys: Vec<Type> = spec.all.iter().map(|c| c.cfg_ty.clone()).collect();

    let req_bindings: Vec<Ident> = spec
        .required
        .iter()
        .map(|c| c.binding_ident.clone())
        .collect();
    let opt_bindings: Vec<Ident> = spec
        .optional
        .iter()
        .map(|c| c.binding_ident.clone())
        .collect();

    let state_alias_idents: Vec<Ident> = spec
        .all
        .iter()
        .map(|c| c.state_alias_ident.clone())
        .collect();
    let state_cache_idents: Vec<Ident> = spec
        .all
        .iter()
        .map(|c| c.state_cache_ident.clone())
        .collect();
    let state_resolve_fn_idents: Vec<Ident> = spec
        .all
        .iter()
        .map(|c| c.state_resolve_fn_ident.clone())
        .collect();

    let req_data_types: Vec<TokenStream2> = req_cfg_tys
        .iter()
        .map(|cfg| quote! { <#cfg as DataTransferConfig>::DataType })
        .collect();
    let opt_data_types: Vec<TokenStream2> = opt_cfg_tys
        .iter()
        .map(|cfg| quote! { <#cfg as DataTransferConfig>::DataType })
        .collect();

    let snapshot_types: Vec<TokenStream2> = req_data_types
        .iter()
        .map(|ty| quote! { Ref<#ty> })
        .chain(opt_data_types.iter().map(|ty| quote! { Option<Ref<#ty>> }))
        .collect();

    let changed_terms: Vec<TokenStream2> = spec
        .all
        .iter()
        .map(|component| {
            let cfg = &component.cfg_ty;
            quote! { Changed<<#cfg as DataTransferConfig>::DataType> }
        })
        .collect();

    let updated_filter = if changed_terms.len() == 1 {
        let only = &changed_terms[0];
        quote! { (With::<#tag_ty>, #only) }
    } else {
        quote! { (With::<#tag_ty>, Or<( #( #changed_terms ),* )>) }
    };

    let req_apply_blocks: Vec<TokenStream2> = spec
        .required
        .iter()
        .map(|component| {
            let cfg_ty = &component.cfg_ty;
            let binding = &component.binding_ident;
            let resolve_fn = &component.state_resolve_fn_ident;

            quote! {
                if is_created || #binding.is_changed() {
                    if let Some(mut state_node) = exporter.#resolve_fn(eid_i64) {
                        let did_change = state_node
                            .bind_mut()
                            .update_from_data::<#cfg_ty>(&*#binding, &mut identity, next_revision);
                        if did_change {
                            changed_candidate = true;
                        }
                    }
                }
            }
        })
        .collect();

    let opt_apply_blocks: Vec<TokenStream2> = spec
        .optional
        .iter()
        .map(|component| {
            let cfg_ty = &component.cfg_ty;
            let binding = &component.binding_ident;
            let resolve_fn = &component.state_resolve_fn_ident;
            let changed_flag = format_ident!("{}_changed", binding);

            quote! {
                let #changed_flag = #binding
                    .as_ref()
                    .map(|value| value.is_changed())
                    .unwrap_or(false);

                if is_created || #changed_flag {
                    if let Some(value) = #binding.as_ref() {
                        if let Some(mut state_node) = exporter.#resolve_fn(eid_i64) {
                            let did_change = state_node
                                .bind_mut()
                                .update_from_data::<#cfg_ty>(&*value, &mut identity, next_revision);
                            if did_change {
                                changed_candidate = true;
                            }
                        }
                    }
                }
            }
        })
        .collect();

    let expanded = quote! {
        pub mod #module_ident {
            use super::*;
            use bevy::ecs::system::SystemParam;
            use bevy::prelude::*;
            use bevy_godot4::prelude::*;
            use godot::prelude::*;
            use std::collections::{HashMap, HashSet};
            use std::marker::PhantomData;

            #[allow(dead_code)]
            fn __export_composed_requires_with_state_node_for_all_dtos() {
                fn assert_cfg<C>()
                where
                    C: DataTransferConfig,
                    <C as DataTransferConfig>::DtoType: WithStateNode,
                {
                }

                #( assert_cfg::<#all_cfg_tys>(); )*
            }

            #( pub type #state_alias_idents = <<#all_cfg_tys as DataTransferConfig>::DtoType as WithStateNode>::StateNode; )*

            #[derive(GodotClass)]
            #[class(base=Node)]
            pub struct #entity_meta_ident {
                #[var] pub entity_id: i64,
                #[var] pub revision: i64,
                #[var] custom_cleanup_enabled: bool,
                #[base] base: Base<Node>,
            }

            #[godot_api]
            impl #entity_meta_ident {
                #[signal]
                fn on_change(revision: i64);

                #[func]
                fn set_custom_cleanup(&mut self, enabled: bool) {
                    self.custom_cleanup_enabled = enabled;
                }

                #[func(virtual)]
                fn on_removed(&mut self) {}
            }

            #[godot_api]
            impl INode for #entity_meta_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        entity_id: -1,
                        revision: -1,
                        custom_cleanup_enabled: false,
                        base,
                    }
                }
            }

            #[derive(GodotClass)]
            #[class(base=Node)]
            pub struct #exporter_ident {
                #[var] scene: Gd<PackedScene>,
                #[var] parent_path: NodePath,
                spawned_roots: HashMap<i64, Gd<Node>>,
                entity_meta_cache: HashMap<i64, Gd<#entity_meta_ident>>,
                revision_cache: HashMap<i64, i64>,
                #( #state_cache_idents: HashMap<i64, Gd<#state_alias_idents>>, )*
                #[base] base: Base<Node>,
            }

            #[godot_api]
            impl #exporter_ident {
                fn parent_node(&mut self) -> Gd<Node> {
                    if self.parent_path.is_empty() {
                        self.base().clone().upcast()
                    } else {
                        self.base().get_node_as::<Node>(&self.parent_path)
                    }
                }

                fn resolve_entity_meta_from_root(&mut self, entity_id: i64) -> Option<Gd<#entity_meta_ident>> {
                    let Some(root) = self.spawned_roots.get(&entity_id).cloned() else {
                        self.entity_meta_cache.remove(&entity_id);
                        return None;
                    };

                    if !root.is_instance_valid() {
                        self.spawned_roots.remove(&entity_id);
                        self.entity_meta_cache.remove(&entity_id);
                        return None;
                    }

                    let meta = self.resolve_entity_meta_from_instance(root.clone(), entity_id);

                    self.entity_meta_cache.insert(entity_id, meta.clone());
                    Some(meta)
                }

                fn resolve_entity_meta_from_instance(&mut self, instance: Gd<Node>, entity_id: i64) -> Gd<#entity_meta_ident> {
                    let root_meta = instance.clone().try_cast::<#entity_meta_ident>().ok();

                    let mut direct_children_meta: Vec<Gd<#entity_meta_ident>> = Vec::new();
                    let child_count = instance.get_child_count();
                    for idx in 0..child_count {
                        let Some(child) = instance.get_child(idx) else {
                            continue;
                        };

                        if let Ok(meta) = child.try_cast::<#entity_meta_ident>() {
                            direct_children_meta.push(meta);
                        }
                    }

                    if root_meta.is_some() && !direct_children_meta.is_empty() {
                        godot_warn!(
                            "Multiple {} nodes found for entity {}; using root-attached one",
                            stringify!(#entity_meta_ident),
                            entity_id
                        );
                    }

                    if direct_children_meta.len() > 1 {
                        godot_warn!(
                            "Multiple direct-child {} nodes found for entity {}; using the first one",
                            stringify!(#entity_meta_ident),
                            entity_id
                        );
                    }

                    if let Some(meta) = root_meta {
                        return meta;
                    }

                    if let Some(meta) = direct_children_meta.into_iter().next() {
                        return meta;
                    }

                    let nested_meta = collect_children::<#entity_meta_ident>(instance.clone(), true);

                    if !nested_meta.is_empty() {
                        panic!(
                            "{} must be attached to scene root or as a direct child of the root for entity {}; deeper nested placement is invalid",
                            stringify!(#entity_meta_ident),
                            entity_id
                        );
                    }

                    panic!(
                        "Spawned scene is missing {} for entity {}; attach it to root or root direct child",
                        stringify!(#entity_meta_ident),
                        entity_id
                    );
                }

                fn ensure_spawned_entity_meta(&mut self, entity_id: i64) -> Option<Gd<#entity_meta_ident>> {
                    if let Some(cached) = self.entity_meta_cache.get(&entity_id) {
                        if cached.is_instance_valid() {
                            return Some(cached.clone());
                        }
                    }

                    if let Some(meta) = self.resolve_entity_meta_from_root(entity_id) {
                        return Some(meta);
                    }

                    if !self.scene.is_instance_valid() {
                        godot_error!("{} scene is not configured; set `scene` on exporter", stringify!(#exporter_ident));
                        return None;
                    }

                    let Some(instance) = self.scene.instantiate() else {
                        godot_error!("Failed to instantiate scene for {}", stringify!(#exporter_ident));
                        return None;
                    };

                    let mut parent = self.parent_node();
                    parent.add_child(&instance);
                    self.spawned_roots.insert(entity_id, instance.clone());

                    let mut meta = self.resolve_entity_meta_from_instance(instance.clone(), entity_id);

                    {
                        let mut m = meta.bind_mut();
                        m.entity_id = entity_id;
                    }

                    self.entity_meta_cache.insert(entity_id, meta.clone());
                    Some(meta)
                }

                fn cleanup_entity(&mut self, entity_id: i64) {
                    let mut custom_cleanup = false;

                    if let Some(mut meta) = self.entity_meta_cache.remove(&entity_id) {
                        if meta.is_instance_valid() {
                            custom_cleanup = meta.bind().custom_cleanup_enabled;
                            if custom_cleanup {
                                meta.bind_mut().on_removed();
                            }
                        }
                    }

                    self.revision_cache.remove(&entity_id);
                    #( self.#state_cache_idents.remove(&entity_id); )*

                    if let Some(mut root) = self.spawned_roots.remove(&entity_id) {
                        if root.is_instance_valid() && !custom_cleanup {
                            root.queue_free();
                        }
                    }
                }

                #[func]
                fn get_entity_or_null(&mut self, entity_id: i64) -> Option<Gd<Node>> {
                    let Some(root) = self.spawned_roots.get(&entity_id).cloned() else {
                        return None;
                    };

                    if root.is_instance_valid() {
                        Some(root)
                    } else {
                        self.spawned_roots.remove(&entity_id);
                        self.entity_meta_cache.remove(&entity_id);
                        self.revision_cache.remove(&entity_id);
                        #( self.#state_cache_idents.remove(&entity_id); )*
                        None
                    }
                }

                #(
                    fn #state_resolve_fn_idents(&mut self, entity_id: i64) -> Option<Gd<#state_alias_idents>> {
                        if let Some(cached) = self.#state_cache_idents.get(&entity_id) {
                            if cached.is_instance_valid() {
                                return Some(cached.clone());
                            }
                        }

                        let Some(meta) = self.ensure_spawned_entity_meta(entity_id) else {
                            self.#state_cache_idents.remove(&entity_id);
                            return None;
                        };

                        let parent = meta.clone().upcast::<Node>();
                        let mut found = collect_children::<#state_alias_idents>(parent, true);

                        if found.len() > 1 {
                            panic!(
                                "Multiple {} nodes found for entity {}. Exported entities must have exactly one state node per capability type.",
                                stringify!(#state_alias_idents),
                                entity_id
                            );
                        }

                        let Some(state) = found.pop() else {
                            self.#state_cache_idents.remove(&entity_id);
                            return None;
                        };

                        self.#state_cache_idents.insert(entity_id, state.clone());
                        Some(state)
                    }
                )*
            }

            #[godot_api]
            impl INode for #exporter_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        scene: PackedScene::new_gd(),
                        parent_path: NodePath::default(),
                        spawned_roots: HashMap::new(),
                        entity_meta_cache: HashMap::new(),
                        revision_cache: HashMap::new(),
                        #( #state_cache_idents: HashMap::new(), )*
                        base,
                    }
                }
            }

            #[derive(Debug, Default)]
            struct #exporter_accessor_impl_ident(Option<Gd<#exporter_ident>>);

            #[derive(SystemParam)]
            struct #exporter_accessor_ident<'w, 's> {
                gd: NonSendMut<'w, #exporter_accessor_impl_ident>,
                phantom: PhantomData<&'s ()>,
            }

            impl #exporter_accessor_ident<'_, '_> {
                fn get(&mut self, scene_tree: &mut SceneTreeSubsystem) -> Option<Gd<#exporter_ident>> {
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

            #[allow(non_snake_case)]
            fn #system_ident(
                created: Query<Entity, Added<#tag_ty>>,
                updated: Query<Entity, #updated_filter>,
                mut removed: RemovedComponents<#tag_ty>,
                snapshot: Query<( #( #snapshot_types, )* ), With<#tag_ty>>,
                mut identity: IdentitySubsystem,
                mut scene_tree: SceneTreeSubsystem,
                mut exporter_accessor: #exporter_accessor_ident,
            ) {
                let any_created = !created.is_empty();
                let any_updated = !updated.is_empty();
                let any_removed = !removed.is_empty();

                if !any_created && !any_updated && !any_removed {
                    return;
                }

                let Some(mut exporter) = exporter_accessor.get(&mut scene_tree) else {
                    return;
                };
                let mut exporter = exporter.bind_mut();

                let mut created_bits: HashSet<u64> = HashSet::new();

                if any_created {
                    for entity in created.iter() {
                        created_bits.insert(entity.to_bits());
                        let entity_id = identity.get_identity(entity);
                        let _ = exporter.ensure_spawned_entity_meta(entity_id);
                    }
                }

                let mut process_entity = |entity: Entity, is_created: bool| {
                    let eid_u64 = entity.to_bits();
                    let eid_i64 = identity.get_identity(entity);

                    let Ok(( #( #req_bindings, )* #( #opt_bindings, )* )) = snapshot.get(entity) else {
                        return;
                    };

                    let Some(mut meta) = exporter.ensure_spawned_entity_meta(eid_i64) else {
                        return;
                    };

                    let next_revision = exporter.revision_cache.get(&eid_i64).copied().unwrap_or(-1) + 1;
                    let mut changed_candidate = false;

                    #( #req_apply_blocks )*
                    #( #opt_apply_blocks )*

                    if changed_candidate {
                        exporter.revision_cache.insert(eid_i64, next_revision);

                        {
                            let mut m = meta.bind_mut();
                            m.revision = next_revision;
                            m.signals().on_change().emit(next_revision);
                        }
                    }
                };

                if any_created {
                    for entity in created.iter() {
                        process_entity(entity, true);
                    }
                }

                if any_updated {
                    for entity in updated.iter() {
                        if created_bits.contains(&entity.to_bits()) {
                            continue;
                        }

                        process_entity(entity, false);
                    }
                }

                if any_removed {
                    for entity in removed.read() {
                        if let Some(entity_id) = identity.try_get_identity(entity) {
                            exporter.cleanup_entity(entity_id);
                        }
                    }
                }
            }

            pub struct #plugin_ident;

            impl Plugin for #plugin_ident {
                fn build(&self, app: &mut App) {
                    app.init_non_send_resource::<#exporter_accessor_impl_ident>();
                    app.add_systems(PostUpdate, #system_ident);
                }
            }
        }

        pub use #module_ident::{
            #entity_meta_ident,
            #exporter_ident,
            #plugin_ident,
            #( #state_alias_idents, )*
        };
    };

    expanded.into()
}
