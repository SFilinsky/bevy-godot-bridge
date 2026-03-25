use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    Ident, Path, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

struct Spec {
    dto: Path,
    state: Ident,
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut dto: Option<Path> = None;
        let mut state: Option<Ident> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "dto" => dto = Some(input.parse()?),
                "state" => state = Some(input.parse()?),
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("unknown key `{other}`; expected `dto` or `state`"),
                    ));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        Ok(Self {
            dto: dto
                .ok_or_else(|| syn::Error::new(Span::call_site(), "missing `dto: SomeDtoType`"))?,
            state: state.ok_or_else(|| {
                syn::Error::new(Span::call_site(), "missing `state: SomeStateNode`")
            })?,
        })
    }
}

pub fn expand(input: TokenStream) -> TokenStream {
    let Spec { dto, state } = parse_macro_input!(input as Spec);

    let expanded = quote! {
        use godot::prelude::*;

        #[derive(GodotClass)]
        #[class(base=Node)]
        pub struct #state {
            entries: std::collections::HashMap<i64, (Gd<#dto>, Gd<#dto>, Gd<#dto>, i64)>,
            #[base] base: Base<Node>,
        }

        impl #state {
            pub fn upsert_from_data<C>(
                &mut self,
                entity_id: i64,
                data: &C::DataType,
                identity: &mut bevy_godot4::prelude::IdentitySubsystem,
                revision: i64,
            ) -> bool
            where
                C: bevy_godot4::prelude::DataTransferConfig<DtoType = #dto>,
                #dto: PartialEq,
            {
                if entity_id < 0 {
                    return false;
                }

                let entry = self.entries.entry(entity_id).or_insert_with(|| {
                    (#dto::new_gd(), #dto::new_gd(), #dto::new_gd(), -1)
                });

                let (curr, prev, spare, updated_revision) = entry;

                if *updated_revision < 0 {
                    C::update_dto(curr, data, identity);
                    C::update_dto(prev, data, identity);
                    C::update_dto(spare, data, identity);
                    *updated_revision = revision;
                    return true;
                }

                C::update_dto(spare, data, identity);

                let same_value = {
                    let curr_bound = curr.bind();
                    let spare_bound = spare.bind();
                    *curr_bound == *spare_bound
                };

                if same_value {
                    return false;
                }

                std::mem::swap(prev, curr);
                std::mem::swap(curr, spare);
                *updated_revision = revision;
                true
            }

            pub fn remove_entity(&mut self, entity_id: i64) {
                self.entries.remove(&entity_id);
            }

            pub fn curr_for(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.entries
                    .get(&entity_id)
                    .map(|(curr, _prev, _spare, _revision)| curr.clone())
            }

            pub fn prev_for(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.entries
                    .get(&entity_id)
                    .map(|(_curr, prev, _spare, _revision)| prev.clone())
            }

            pub fn updated_revision_for(&self, entity_id: i64) -> i64 {
                self.entries
                    .get(&entity_id)
                    .map(|(_curr, _prev, _spare, revision)| *revision)
                    .unwrap_or(-1)
            }
        }

        #[godot_api]
        impl #state {
            #[func]
            fn resolve(owner: Gd<Node>) -> Option<Gd<#state>> {
                if !owner.is_instance_valid() {
                    return None;
                }

                let mut found = bevy_godot4::prelude::collect_children::<#state>(owner, false);

                if found.len() > 1 {
                    panic!(
                        "Multiple {} nodes found under the owner node; expected exactly one singleton store",
                        stringify!(#state)
                    );
                }

                found.pop()
            }

            #[func]
            fn implements(&self, entity_id: i64) -> bool {
                self.entries.contains_key(&entity_id)
            }

            #[func]
            fn get_state(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.curr_for(entity_id)
            }

            #[func]
            fn get_curr(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.curr_for(entity_id)
            }

            #[func]
            fn get_prev(&self, entity_id: i64) -> Option<Gd<#dto>> {
                self.prev_for(entity_id)
            }

            #[func]
            fn is_updated_in_revision(&self, entity_id: i64, revision: i64) -> bool {
                self.updated_revision_for(entity_id) == revision
            }

            #[func]
            fn get_capability(entity_meta: Gd<bevy_godot4::prelude::EntityMeta>) -> Option<Gd<#state>> {
                if !entity_meta.is_instance_valid() {
                    return None;
                }

                let entity_id = {
                    let bound = entity_meta.bind();
                    bound.entity_id
                };

                if entity_id < 0 {
                    return None;
                }

                let tree = entity_meta.get_tree()?;
                let root = tree.get_root()?;
                let owner = root.try_get_node_as::<Node>("BevyAppSingleton")?;
                let store = Self::resolve(owner)?;

                if store.bind().implements(entity_id) {
                    Some(store)
                } else {
                    None
                }
            }

            #[func]
            fn has_capability(entity_meta: Gd<bevy_godot4::prelude::EntityMeta>) -> bool {
                Self::get_capability(entity_meta).is_some()
            }
        }

        #[godot_api]
        impl INode for #state {
            fn init(base: Base<Node>) -> Self {
                Self {
                    entries: std::collections::HashMap::new(),
                    base,
                }
            }
        }

        impl bevy_godot4::prelude::WithStateNode for #dto {
            type StateNode = #state;
        }
    };

    expanded.into()
}
