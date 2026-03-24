use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, Path, Result, Token,
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
            curr: Option<Gd<#dto>>,
            prev: Option<Gd<#dto>>,
            spare: Option<Gd<#dto>>,
            #[var] pub updated_revision: i64,
            #[base] base: Base<Node>,
        }

        impl #state {
            pub fn update_from_data<C>(
                &mut self,
                data: &C::DataType,
                identity: &mut bevy_godot4::prelude::IdentitySubsystem,
                revision: i64,
            ) -> bool
            where
                C: bevy_godot4::prelude::DataTransferConfig<DtoType = #dto>,
                #dto: PartialEq,
            {
                if self.curr.is_none() {
                    self.curr = Some(#dto::new_gd());
                }
                if self.prev.is_none() {
                    self.prev = Some(#dto::new_gd());
                }
                if self.spare.is_none() {
                    self.spare = Some(#dto::new_gd());
                }

                if self.updated_revision < 0 {
                    if let Some(curr) = self.curr.as_mut() {
                        C::update_dto(curr, data, identity);
                    }

                    self.updated_revision = revision;
                    return true;
                }

                if let Some(spare) = self.spare.as_mut() {
                    C::update_dto(spare, data, identity);
                }

                let same_value = if let (Some(curr), Some(spare)) = (self.curr.as_ref(), self.spare.as_ref()) {
                    let curr_bound = curr.bind();
                    let spare_bound = spare.bind();
                    *curr_bound == *spare_bound
                } else {
                    false
                };

                if same_value {
                    return false;
                }

                std::mem::swap(&mut self.prev, &mut self.curr);
                std::mem::swap(&mut self.curr, &mut self.spare);
                self.updated_revision = revision;
                true
            }

            pub fn curr(&self) -> Option<Gd<#dto>> {
                self.curr.clone()
            }

            pub fn prev(&self) -> Option<Gd<#dto>> {
                self.prev.clone()
            }
        }

        #[godot_api]
        impl #state {
            #[func]
            fn get_capability(entity_meta: Gd<bevy_godot4::prelude::EntityMeta>) -> Option<Gd<#state>> {
                if !entity_meta.is_instance_valid() {
                    return None;
                }

                let parent = entity_meta.upcast::<Node>();
                let mut found = bevy_godot4::prelude::collect_children::<#state>(parent, true);

                if found.len() > 1 {
                    panic!(
                        "Multiple {} capability nodes found under EntityMeta. Expected exactly one.",
                        stringify!(#state)
                    );
                }

                found.pop()
            }

            #[func]
            fn has_capability(entity_meta: Gd<bevy_godot4::prelude::EntityMeta>) -> bool {
                Self::get_capability(entity_meta).is_some()
            }

            #[func]
            fn get_curr(&self) -> Option<Gd<#dto>> {
                self.curr()
            }

            #[func]
            fn get_prev(&self) -> Option<Gd<#dto>> {
                self.prev()
            }
        }

        #[godot_api]
        impl INode for #state {
            fn init(base: Base<Node>) -> Self {
                Self {
                    curr: None,
                    prev: None,
                    spare: None,
                    updated_revision: -1,
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
