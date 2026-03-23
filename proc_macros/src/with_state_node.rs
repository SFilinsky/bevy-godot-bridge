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
            #[var] pub updated_revision: i64,
            #[base] base: Base<Node>,
        }

        impl #state {
            pub fn apply_revision(&mut self, curr: Gd<#dto>, revision: i64) {
                self.prev = self.curr.take();
                self.curr = Some(curr);
                self.updated_revision = revision;
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
