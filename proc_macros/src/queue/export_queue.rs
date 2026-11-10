//! Code used by `export_queue!`.
//!
//! The macro generates only the concrete Godot class. Source collection uses
//! normal generic Rust plugins so most outbound queue code stays out of the
//! proc-macro crate.

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{
    Ident, Path, Result, Token,
    parse::{Parse, ParseStream},
};

const CFG_SUFFIX: &str = "TransferConfig";

struct Spec {
    config: Path,
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let key: Ident = input.parse()?;
        input.parse::<Token![:]>()?;

        if key != "config" {
            return Err(syn::Error::new_spanned(
                key,
                "unknown key; expected `config`",
            ));
        }

        let config = input.parse()?;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
        }
        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), "expected only `config: ...`"));
        }

        Ok(Self { config })
    }
}

fn queue_name_from_config(config: &Path) -> syn::Result<Ident> {
    let Some(config_ident) = config.segments.last().map(|segment| segment.ident.clone()) else {
        return Err(syn::Error::new(
            config.span(),
            "config must be a type path like `ResourceTransactionTransferConfig`",
        ));
    };

    let config_name = config_ident.to_string();
    let Some(domain_name) = config_name.strip_suffix(CFG_SUFFIX) else {
        return Err(syn::Error::new(
            config_ident.span(),
            format!(
                "config type must end with `{CFG_SUFFIX}` (for example, `ResourceTransaction{CFG_SUFFIX}`)"
            ),
        ));
    };

    if domain_name.is_empty() {
        return Err(syn::Error::new(
            config_ident.span(),
            format!("config type name cannot be just `{CFG_SUFFIX}`"),
        ));
    }

    Ok(format_ident!(
        "{domain_name}ExportQueue",
        span = Span::call_site()
    ))
}

pub fn expand(input: TokenStream) -> TokenStream {
    let spec = syn::parse_macro_input!(input as Spec);
    expand_spec(spec).into()
}

fn expand_spec(spec: Spec) -> TokenStream2 {
    let config = spec.config;
    let queue_name = queue_name_from_config(&config).unwrap();
    let module_name = format_ident!("__{}_export_queue", queue_name);

    quote! {
        #[allow(non_snake_case)]
        mod #module_name {
            use bevy_godot4::prelude::{BevyApp, QueueSource, UpdateReceiver, UpdateSubscription};
            use godot::classes::{Node, RefCounted};
            use godot::global::godot_error;
            use godot::obj::{Base, Gd, WithBaseField};
            use godot::prelude::{GodotClass, godot_api};
            type Config = super::#config;

            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #queue_name {
                source: Option<QueueSource<Config>>,
                base: Base<RefCounted>,
            }

            #[godot_api]
            impl #queue_name {
                #[func]
                pub fn bind_bevy_app(&mut self, app: Gd<BevyApp>) {
                    // Dropping an old binding cancels its subscribers before attaching anew.
                    self.source = None;
                    self.source = QueueSource::bind(app, self.to_gd().instance_id().to_i64());
                    if self.source.is_none() { godot_error!("{} needs an initialized app with its export plugin", stringify!(#queue_name)); }
                }

                #[func]
                pub fn subscribe(&self, owner: Gd<Node>, receiver: Gd<UpdateReceiver>) -> Option<Gd<UpdateSubscription>> {
                    self.source.as_ref()?.subscribe(owner, receiver)
                }

                #[func]
                pub fn is_attached(&self) -> bool { self.source.as_ref().is_some_and(QueueSource::is_attached) }
            }
        }
        pub use #module_name::#queue_name;
    }
}

#[cfg(test)]
mod tests {
    use super::{Spec, expand_spec};
    use syn::{File, parse_quote, parse2};

    #[test]
    fn generated_queue_uses_the_common_source_and_receiver_contract() {
        let output = expand_spec(Spec {
            config: parse_quote!(ResourceTransactionTransferConfig),
        });
        parse2::<File>(output.clone()).expect("queue wrapper must form valid Rust");
        let text = output.to_string();
        assert!(text.contains("QueueSource :: bind"));
        assert!(text.contains("UpdateReceiver"));
        assert!(!text.contains("values_available"));
        assert!(!text.contains("call_deferred"));
    }
}
