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
    state_store_var_ident: Ident,
    state_store_bind_ident: Ident,
}

struct Spec {
    tag: Path,
    module_ident: Ident,
    exporter_ident: Ident,
    spawner_ident: Ident,
    plugin_ident: Ident,
    system_ident: Ident,
    exporter_accessor_ident: Ident,
    exporter_accessor_impl_ident: Ident,
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
        let spawner_ident = format_ident!("{}EntitySpawner", name_camel);
        let plugin_ident = format_ident!("{}EntityExportPlugin", name_camel);
        let system_ident = format_ident!("export_{}_entity_changes", name_snake);
        let exporter_accessor_ident = format_ident!("{}ExporterAccessor", name_camel);
        let exporter_accessor_impl_ident = format_ident!("{}ExporterAccessorImpl", name_camel);

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
            let state_store_var_ident = format_ident!("{}_state_store", field_ident);
            let state_store_bind_ident = format_ident!("{}_state_store_bind", field_ident);

            let component = ComponentSpec {
                cfg_ty,
                binding_ident,
                state_alias_ident,
                state_store_var_ident,
                state_store_bind_ident,
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
            spawner_ident,
            plugin_ident,
            system_ident,
            exporter_accessor_ident,
            exporter_accessor_impl_ident,
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
    let spawner_ident = &spec.spawner_ident;
    let plugin_ident = &spec.plugin_ident;
    let system_ident = &spec.system_ident;
    let exporter_accessor_ident = &spec.exporter_accessor_ident;
    let exporter_accessor_impl_ident = &spec.exporter_accessor_impl_ident;

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

    let opt_removed_reader_idents: Vec<Ident> = spec
        .optional
        .iter()
        .map(|component| format_ident!("{}_removed", component.binding_ident))
        .collect();

    let any_opt_removed_expr = if opt_removed_reader_idents.is_empty() {
        quote! { false }
    } else {
        quote! { false #( || !#opt_removed_reader_idents.is_empty() )* }
    };

    let req_apply_blocks: Vec<TokenStream2> = spec
        .required
        .iter()
        .map(|component| {
            let cfg_ty = &component.cfg_ty;
            let binding = &component.binding_ident;
            let state_store_bind = &component.state_store_bind_ident;

            quote! {
                if is_created || #binding.is_changed() {
                    let did_change = #state_store_bind
                        .upsert_from_data::<#cfg_ty>(eid_i64, &*#binding, &mut identity, next_revision);
                    if did_change {
                        changed_candidate = true;
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
            let state_store_bind = &component.state_store_bind_ident;
            let changed_flag = format_ident!("{}_changed", binding);

            quote! {
                let #changed_flag = #binding
                    .as_ref()
                    .map(|value| value.is_changed())
                    .unwrap_or(false);

                if is_created || #changed_flag {
                    if let Some(value) = #binding.as_ref() {
                        let did_change = #state_store_bind
                            .upsert_from_data::<#cfg_ty>(eid_i64, &*value, &mut identity, next_revision);
                        if did_change {
                            changed_candidate = true;
                        }
                    } else if !is_created {
                        if #state_store_bind.implements(eid_i64) {
                            #state_store_bind.remove_entity(eid_i64);
                            changed_candidate = true;
                        }
                    }
                }
            }
        })
        .collect();

    let state_store_resolve_blocks: Vec<TokenStream2> = spec
        .all
        .iter()
        .map(|component| {
            let state_store = &component.state_store_var_ident;
            let state_alias = &component.state_alias_ident;

            quote! {
                let mut #state_store = #state_alias::resolve(state_store_owner.clone()).unwrap_or_else(|| {
                    panic!(
                        "{} failed to resolve {} singleton store before export tick",
                        stringify!(#exporter_ident),
                        stringify!(#state_alias)
                    )
                });
            }
        })
        .collect();

    let state_store_entity_bind_blocks: Vec<TokenStream2> = spec
        .all
        .iter()
        .map(|component| {
            let state_store = &component.state_store_var_ident;
            let state_store_bind = &component.state_store_bind_ident;

            quote! {
                let mut #state_store_bind = #state_store.bind_mut();
            }
        })
        .collect();

    let state_remove_blocks: Vec<TokenStream2> = spec
        .all
        .iter()
        .map(|component| {
            let state_store = &component.state_store_var_ident;

            quote! {
                #state_store.bind_mut().remove_entity(entity_id);
            }
        })
        .collect();

    let opt_removed_apply_blocks: Vec<TokenStream2> = spec
        .optional
        .iter()
        .zip(opt_removed_reader_idents.iter())
        .map(|(component, removed_reader)| {
            let state_store = &component.state_store_var_ident;

            quote! {
                for entity in #removed_reader.read() {
                    let Some(entity_id) = identity.try_get_identity(entity) else {
                        continue;
                    };

                    if removed_entity_ids.contains(&entity_id) || emitted_entity_ids.contains(&entity_id) {
                        continue;
                    }

                    let did_remove = {
                        let mut state_store_bind = #state_store.bind_mut();
                        if state_store_bind.implements(entity_id) {
                            state_store_bind.remove_entity(entity_id);
                            true
                        } else {
                            false
                        }
                    };

                    if did_remove {
                        let next_revision = exporter.revision_cache.get(&entity_id).copied().unwrap_or(-1) + 1;
                        exporter.revision_cache.insert(entity_id, next_revision);
                        exporter.signals().on_updated().emit(entity_id, next_revision);
                        emitted_entity_ids.insert(entity_id);
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
            pub struct #exporter_ident {
                revision_cache: HashMap<i64, i64>,
                #[base] base: Base<Node>,
            }

            #[godot_api]
            impl #exporter_ident {
                #[signal]
                fn on_created(entity_id: i64, revision: i64);

                #[signal]
                fn on_updated(entity_id: i64, revision: i64);

                #[signal]
                fn on_removed(entity_id: i64);

                #[func]
                fn resolve(owner: Gd<Node>) -> Option<Gd<#exporter_ident>> {
                    if !owner.is_instance_valid() {
                        return None;
                    }

                    let Ok(app) = BevyApp::resolve(&owner) else {
                        return None;
                    };

                    app.try_get_node_as::<#exporter_ident>(stringify!(#exporter_ident))
                }

                fn cleanup_entity(&mut self, entity_id: i64) {
                    self.revision_cache.remove(&entity_id);
                }

                #[func]
                fn get_known_entity_ids(&self) -> PackedInt64Array {
                    let mut out = PackedInt64Array::new();
                    for entity_id in self.revision_cache.keys() {
                        out.push(*entity_id);
                    }
                    out
                }
            }

            #[godot_api]
            impl INode for #exporter_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        revision_cache: HashMap::new(),
                        base,
                    }
                }
            }

            #[derive(GodotClass)]
            #[class(base=Node)]
            pub struct #spawner_ident {
                #[export]
                #[var]
                scene: Option<Gd<PackedScene>>,
                #[export]
                #[var]
                parent_path: NodePath,
                spawned_roots: HashMap<i64, Gd<Node>>,
                entity_meta_cache: HashMap<i64, Gd<EntityMeta>>,
                entity_registry: Option<Gd<EntityRegistry>>,
                parent_node_cache: Option<Gd<Node>>,
                exported_group_cache: Option<Gd<Node>>,
                #[base] base: Base<Node>,
            }

            #[godot_api]
            impl #spawner_ident {
                #[signal]
                fn on_spawned(entity_id: i64, revision: i64);

                #[signal]
                fn on_despawned(entity_id: i64);

                #[func]
                fn resolve(owner: Gd<Node>) -> Option<Gd<#spawner_ident>> {
                    if !owner.is_instance_valid() {
                        return None;
                    }

                    let Ok(app) = BevyApp::resolve(&owner) else {
                        return None;
                    };

                    app.try_get_node_as::<#spawner_ident>(stringify!(#spawner_ident))
                }

                fn ensure_child_root(parent: &mut Gd<Node>, child_name: &str) -> Gd<Node> {
                    if let Some(existing) = parent.try_get_node_as::<Node>(child_name) {
                        return existing;
                    }

                    let mut node = Node::new_alloc();
                    node.set_name(child_name);
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
                                    stringify!(#spawner_ident)
                                );
                                return None;
                            }

                            let mut host = parent.clone();
                            let group = Self::ensure_child_root(&mut host, "Exported");
                            self.exported_group_cache = Some(group.clone());
                            return Some(group);
                        }

                        godot_error!(
                            "{} parent node became invalid after initial resolve",
                            stringify!(#spawner_ident)
                        );
                        return None;
                    }

                    godot_error!(
                        "{} parent node was not resolved during ready()",
                        stringify!(#spawner_ident)
                    );
                    None
                }

                fn resolve_entity_meta_from_root(&mut self, entity_id: i64) -> Option<Gd<EntityMeta>> {
                    let Some(root) = self.spawned_roots.get(&entity_id).cloned() else {
                        self.entity_meta_cache.remove(&entity_id);
                        return None;
                    };

                    if !root.is_instance_valid() {
                        self.spawned_roots.remove(&entity_id);
                        self.entity_meta_cache.remove(&entity_id);
                        return None;
                    }

                    let meta = EntityMeta::resolve_from_scene_root(root.clone(), entity_id);
                    self.entity_meta_cache.insert(entity_id, meta.clone());
                    Some(meta)
                }

                fn ensure_spawned_entity_meta(&mut self, entity_id: i64) -> Option<Gd<EntityMeta>> {
                    if let Some(cached) = self.entity_meta_cache.get(&entity_id) {
                        if cached.is_instance_valid() {
                            return Some(cached.clone());
                        }
                    }

                    if let Some(meta) = self.resolve_entity_meta_from_root(entity_id) {
                        return Some(meta);
                    }

                    let Some(scene) = self.scene.as_ref() else {
                        return None;
                    };

                    if !scene.is_instance_valid() {
                        panic!("{} scene is invalid; set a valid `scene` on spawner", stringify!(#spawner_ident));
                    }

                    let Some(instance) = scene.instantiate() else {
                        panic!("Failed to instantiate scene for {}", stringify!(#spawner_ident));
                    };

                    let mut meta = EntityMeta::resolve_from_scene_root(instance.clone(), entity_id);
                    meta.bind_mut().assign_entity_id(entity_id);

                    let Some(mut parent) = self.parent_node() else {
                        return None;
                    };
                    parent.add_child(&instance);
                    self.spawned_roots.insert(entity_id, instance.clone());

                    self.register_entity_meta(entity_id, meta.clone());

                    self.entity_meta_cache.insert(entity_id, meta.clone());
                    Some(meta)
                }

                fn register_entity_meta(&mut self, entity_id: i64, meta: Gd<EntityMeta>) {
                    let Some(mut registry) = self.entity_registry.as_ref().cloned() else {
                        godot_error!(
                            "{} cannot register entity {}: EntityRegistry was not resolved during ready()",
                            stringify!(#spawner_ident),
                            entity_id
                        );
                        return;
                    };

                    if !registry.is_instance_valid() {
                        godot_error!(
                            "{} cannot register entity {}: cached EntityRegistry became invalid",
                            stringify!(#spawner_ident),
                            entity_id
                        );
                        return;
                    }

                    registry.bind_mut().register_entity_meta(entity_id, meta);
                }

                fn unregister_entity_meta(&mut self, entity_id: i64) {
                    let Some(mut registry) = self.entity_registry.as_ref().cloned() else {
                        return;
                    };

                    if !registry.is_instance_valid() {
                        return;
                    }

                    registry.bind_mut().unregister_entity_meta(entity_id);
                }

                fn cleanup_entity(&mut self, entity_id: i64) {
                    let mut custom_cleanup = false;

                    if let Some(mut meta) = self.entity_meta_cache.remove(&entity_id) {
                        if meta.is_instance_valid() {
                            custom_cleanup = meta.bind().is_custom_cleanup_enabled();
                            meta.bind_mut().signals().on_despawning().emit(entity_id);
                        }
                    }

                    self.unregister_entity_meta(entity_id);

                    if let Some(mut root) = self.spawned_roots.remove(&entity_id) {
                        if root.is_instance_valid() && !custom_cleanup {
                            root.queue_free();
                        }
                    }
                }

                #[func]
                fn get_spawned_entity_ids(&self) -> PackedInt64Array {
                    let mut out = PackedInt64Array::new();
                    for entity_id in self.spawned_roots.keys() {
                        out.push(*entity_id);
                    }
                    out
                }

                #[func]
                fn _on_created(&mut self, entity_id: i64, revision: i64) {
                    let Some(mut meta) = self.ensure_spawned_entity_meta(entity_id) else {
                        return;
                    };

                    let mut meta = meta.bind_mut();
                    meta.revision = revision;
                    meta.signals().on_change().emit(revision);
                    self.signals().on_spawned().emit(entity_id, revision);
                }

                #[func]
                fn _on_updated(&mut self, entity_id: i64, revision: i64) {
                    let Some(mut meta) = self.ensure_spawned_entity_meta(entity_id) else {
                        return;
                    };

                    let mut meta = meta.bind_mut();
                    meta.revision = revision;
                    meta.signals().on_change().emit(revision);
                }

                #[func]
                fn _on_removed(&mut self, entity_id: i64) {
                    self.cleanup_entity(entity_id);
                    self.signals().on_despawned().emit(entity_id);
                }
            }

            #[godot_api]
            impl INode for #spawner_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        scene: None,
                        parent_path: NodePath::default(),
                        spawned_roots: HashMap::new(),
                        entity_meta_cache: HashMap::new(),
                        entity_registry: None,
                        parent_node_cache: None,
                        exported_group_cache: None,
                        base,
                    }
                }

                fn ready(&mut self) {
                    let host = self.base().clone().upcast::<Node>();
                    let Ok(mut owner) = BevyApp::resolve(&host) else {
                        panic!("{} failed to connect: BevyApp node is missing", stringify!(#spawner_ident));
                    };
                    let Some(mut exporter) = owner.try_get_node_as::<#exporter_ident>(stringify!(#exporter_ident)) else {
                        panic!("{} failed to connect: {} node is missing", stringify!(#spawner_ident), stringify!(#exporter_ident));
                    };

                    let owner_node: Gd<Node> = owner.clone().upcast();
                    self.entity_registry = EntityRegistry::resolve(&owner_node);
                    if self.entity_registry.is_none() {
                        godot_error!(
                            "{} failed to resolve EntityRegistry under BevyApp; spawned entities will not be registered",
                            stringify!(#spawner_ident)
                        );
                    }

                    if !self.parent_path.is_empty() {
                        self.parent_node_cache = Some(self.base().get_node_as::<Node>(&self.parent_path));
                    } else {
                        self.parent_node_cache = Some(owner.bind().resolve_node_host());
                    }

                    let created = Callable::from_object_method(&self.to_gd(), "_on_created");
                    if !exporter.is_connected("on_created", &created) {
                        exporter.connect("on_created", &created);
                    }

                    let updated = Callable::from_object_method(&self.to_gd(), "_on_updated");
                    if !exporter.is_connected("on_updated", &updated) {
                        exporter.connect("on_updated", &updated);
                    }

                    let removed = Callable::from_object_method(&self.to_gd(), "_on_removed");
                    if !exporter.is_connected("on_removed", &removed) {
                        exporter.connect("on_removed", &removed);
                    }
                }
            }

            #[derive(Debug, Default)]
            struct #exporter_accessor_impl_ident {
                node: Option<Gd<#exporter_ident>>,
                had_report_missing: bool,
                had_report_invalid: bool,
            }

            #[derive(SystemParam)]
            struct #exporter_accessor_ident<'w, 's> {
                gd: NonSendMut<'w, #exporter_accessor_impl_ident>,
                phantom: PhantomData<&'s ()>,
            }

            impl #exporter_accessor_ident<'_, '_> {
                fn get(&mut self, app: &mut BevyAppSubsystem) -> Option<Gd<#exporter_ident>> {
                    if let Some(cached) = self.gd.node.as_ref() {
                        if cached.is_instance_valid() {
                            return Some(cached.clone());
                        }

                        if !self.gd.had_report_invalid {
                            godot_error!(
                                "{} accessor: cached exporter became invalid after initial resolve",
                                stringify!(#exporter_ident)
                            );
                            self.gd.had_report_invalid = true;
                        }
                        return None;
                    }

                    if self.gd.had_report_missing {
                        return None;
                    }

                    let host = app.host_node();

                    let Some(exporter) = host.try_get_node_as::<#exporter_ident>(stringify!(#exporter_ident)) else {
                        self.gd.node = None;
                        self.gd.had_report_missing = true;
                        godot_error!(
                            "{} accessor: failed initial resolve under BevyApp host",
                            stringify!(#exporter_ident)
                        );
                        return None;
                    };

                    self.gd.node = Some(exporter.clone());
                    Some(exporter)
                }
            }

            #[allow(non_snake_case)]
            fn #system_ident(
                created: Query<Entity, Added<#tag_ty>>,
                updated: Query<Entity, #updated_filter>,
                mut removed: RemovedComponents<#tag_ty>,
                #( mut #opt_removed_reader_idents: RemovedComponents<<#opt_cfg_tys as DataTransferConfig>::DataType>, )*
                snapshot: Query<( #( #snapshot_types, )* ), With<#tag_ty>>,
                mut identity: IdentitySubsystem,
                mut app: BevyAppSubsystem,
                mut exporter_accessor: #exporter_accessor_ident,
            ) {
                let any_created = !created.is_empty();
                let any_updated = !updated.is_empty();
                let any_removed = !removed.is_empty();
                let any_opt_removed = #any_opt_removed_expr;

                if !any_created && !any_updated && !any_removed && !any_opt_removed {
                    return;
                }

                let Some(mut exporter) = exporter_accessor.get(&mut app) else {
                    return;
                };
                let mut exporter = exporter.bind_mut();
                let state_store_owner: Gd<Node> = app.host_node();
                #( #state_store_resolve_blocks )*

                let mut created_bits: HashSet<u64> = HashSet::new();

                if any_created {
                    for entity in created.iter() {
                        created_bits.insert(entity.to_bits());
                    }
                }

                let mut emitted_entity_ids: HashSet<i64> = HashSet::new();

                let mut process_entity = |entity: Entity, is_created: bool| {
                    let eid_i64 = identity.get_identity(entity);

                    let Ok(( #( #req_bindings, )* #( #opt_bindings, )* )) = snapshot.get(entity) else {
                        return;
                    };

                    let next_revision = exporter.revision_cache.get(&eid_i64).copied().unwrap_or(-1) + 1;
                    let mut changed_candidate = false;

                    {
                        #( #state_store_entity_bind_blocks )*
                        #( #req_apply_blocks )*
                        #( #opt_apply_blocks )*
                    }

                    if is_created || changed_candidate {
                        exporter.revision_cache.insert(eid_i64, next_revision);
                        emitted_entity_ids.insert(eid_i64);

                        if is_created {
                            exporter.signals().on_created().emit(eid_i64, next_revision);
                        } else {
                            exporter.signals().on_updated().emit(eid_i64, next_revision);
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

                let mut removed_entity_ids: HashSet<i64> = HashSet::new();

                if any_removed {
                    for entity in removed.read() {
                        if let Some(entity_id) = identity.try_get_identity(entity) {
                            removed_entity_ids.insert(entity_id);
                            #( #state_remove_blocks )*
                            exporter.cleanup_entity(entity_id);
                            exporter.signals().on_removed().emit(entity_id);
                        }
                    }
                }

                #( #opt_removed_apply_blocks )*
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
            #exporter_ident,
            #spawner_ident,
            #plugin_ident,
            #( #state_alias_idents, )*
        };
    };

    expanded.into()
}
