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

    Ok(format_ident!("{domain_name}ExportQueue", span = Span::call_site()))
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
            use bevy::prelude::World;
            use godot::builtin::{Array, Callable};
            use godot::classes::RefCounted;
            use godot::global::godot_error;
            use godot::obj::Base;
            use godot::prelude::*;

            use bevy_godot4::prelude::{BevyApp, DataTransferConfig};

            type __Config = super::#config;
            type __Dto = <__Config as DataTransferConfig>::DtoType;

            /// Godot-facing access to values waiting for one Bevy app.
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #queue_name {
                bevy_app: Option<Gd<BevyApp>>,
                #[base]
                base: Base<RefCounted>,
            }

            #[godot_api]
            impl #queue_name {
                #[signal]
                fn values_available();

                /// Connects this queue to the Bevy app that owns its values.
                #[func]
                pub fn bind_bevy_app(&mut self, mut app: Gd<BevyApp>) {
                    let notification_callback =
                        Callable::from_object_method(&self.to_gd(), "_emit_values_available");
                    self.bevy_app = Some(app.clone());
                    app.bind_mut().with_world_mut(|world: &mut World| {
                        bevy_godot4::export_queue::set_export_queue_notification_callback::<__Config>(
                            world,
                            notification_callback,
                        );
                    });
                }

                /// Returns every value Bevy exported since the previous call.
                #[func]
                pub fn drain(&mut self) -> Array<Gd<__Dto>> {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!(
                            "[ExportQueue:{}] drain() called before bind_bevy_app()",
                            stringify!(#config),
                        );
                        return Array::new();
                    };

                    let mut dto_list = Array::new();
                    app.bind_mut().with_world_mut(|world: &mut World| {
                        dto_list = bevy_godot4::export_queue::drain_to_dtos::<__Config>(world);
                    });
                    dto_list
                }

                /// Returns whether this queue has a Bevy app to read from.
                #[func]
                pub fn is_attached(&self) -> bool {
                    self.bevy_app.is_some()
                }

                #[func]
                fn _emit_values_available(&mut self) {
                    self.signals().values_available().emit();
                }
            }
        }

        pub use #module_name::#queue_name;
    }
}

#[cfg(test)]
mod tests {
    use super::{Spec, expand_spec};
    use syn::parse_quote;

    #[test]
    fn generated_queue_calls_the_shared_drain_helper() {
        let output = expand_spec(Spec {
            config: parse_quote!(ResourceTransactionTransferConfig),
        })
        .to_string();

        assert!(output.contains("drain_to_dtos"));
        assert!(output.contains("ResourceTransactionExportQueue"));
        assert!(output.contains("values_available"));
        assert!(output.contains("set_export_queue_notification_callback"));
    }
}
