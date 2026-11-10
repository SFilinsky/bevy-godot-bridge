//! Generate a concrete Godot queue and plugin name for one transfer config.
//! Request storage and Bevy delivery live in the shared `ImportQueue` implementation.

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{
    Ident, Path, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

const CFG_SUFFIX: &str = "TransferConfig";

/// Returns (domain_ident, domain_snake) from config path, enforcing `<Domain>TransferConfig`.
fn derive_domain_from_config(cfg_path: &Path) -> syn::Result<(Ident, String)> {
    let cfg_ident = last_path_ident(cfg_path).ok_or_else(|| {
        syn::Error::new(
            cfg_path.span(),
            "config must be a type path like `FooTransferConfig`",
        )
    })?;

    let cfg_name = cfg_ident.to_string();
    if !cfg_name.ends_with(CFG_SUFFIX) {
        return Err(syn::Error::new(
            cfg_ident.span(),
            format!(
                "config type must end with `{}` (e.g. `InitializePositionIntention{}`)",
                CFG_SUFFIX, CFG_SUFFIX
            ),
        ));
    }

    let domain_name = cfg_name.strip_suffix(CFG_SUFFIX).unwrap().to_string();

    if domain_name.is_empty() {
        return Err(syn::Error::new(
            cfg_ident.span(),
            format!("config type name cannot be just `{}`", CFG_SUFFIX),
        ));
    }

    let domain_ident = Ident::new(&domain_name, Span::call_site());
    let domain_snake = to_snake(&domain_name);

    Ok((domain_ident, domain_snake))
}

// -----------------------------------------------------------------------------
// Parsing
// -----------------------------------------------------------------------------

struct Spec {
    config: Path,
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut config: Option<Path> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "config" => config = Some(input.parse()?),
                other => {
                    return Err(syn::Error::new_spanned(
                        key,
                        format!("unknown key `{other}`; expected `config`"),
                    ));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        let config =
            config.ok_or_else(|| syn::Error::new(input.span(), "missing `config: ...`"))?;
        Ok(Spec { config })
    }
}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

fn last_path_ident(p: &Path) -> Option<Ident> {
    p.segments.last().map(|s| s.ident.clone())
}

fn to_snake(s: &str) -> String {
    let mut out = String::new();
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() {
            if i != 0 {
                out.push('_');
            }
            out.extend(ch.to_lowercase());
        } else {
            out.push(ch);
        }
    }
    out
}

// -----------------------------------------------------------------------------
// expand
// -----------------------------------------------------------------------------

pub fn expand(input: TokenStream) -> TokenStream {
    let spec = parse_macro_input!(input as Spec);
    let cfg_path = spec.config;
    let (domain_ident, domain_snake) = match derive_domain_from_config(&cfg_path) {
        Ok(names) => names,
        Err(error) => return error.into_compile_error().into(),
    };
    let queue_ident = format_ident!("{}Queue", domain_ident);
    let plugin_ident = format_ident!("{}ImportPlugin", domain_ident);
    let module_ident = format_ident!("__{}_import_queue", domain_snake);

    // Godot needs concrete registered types; shared Rust code owns their behavior.
    let expanded: TokenStream2 = quote! {
        #[allow(non_snake_case)]
        mod #module_ident {
            use bevy::prelude::{App, Plugin};
            use bevy_godot4::prelude::{DtoOf, BevyApp, DataTransferConfig, ImportQueue, ImportQueuePlugin};
            use godot::builtin::Array;
            use godot::classes::RefCounted;
            use godot::obj::{Base, Gd};
            use godot::prelude::{GodotClass, godot_api};

            type Config = super::#cfg_path;
            type Dto = DtoOf<Config>;

            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #queue_ident {
                queue: ImportQueue<Config>,
                base: Base<RefCounted>,
            }

            #[godot_api]
            impl #queue_ident {
                #[func]
                pub fn bind_bevy_app(&mut self, app: Gd<BevyApp>) {
                    self.queue.bind_bevy_app(app);
                }

                #[func]
                pub fn enqueue(&mut self, dto: Gd<Dto>) {
                    self.queue.enqueue(dto);
                }

                pub fn enqueue_many(&mut self, dto_list: Vec<Gd<Dto>>) {
                    self.queue.enqueue_many(dto_list);
                }

                #[func]
                pub fn enqueue_batch(&mut self, dto_list: Array<Gd<Dto>>) {
                    self.queue.enqueue_many(dto_list.iter_shared());
                }

                #[func]
                pub fn flush(&mut self) {
                    self.queue.flush();
                }

                #[func]
                pub fn is_attached(&self) -> bool {
                    self.queue.is_attached()
                }
            }

            pub struct #plugin_ident;

            impl Plugin for #plugin_ident {
                fn build(&self, app: &mut App) {
                    app.add_plugins(ImportQueuePlugin::<Config>::default());
                }
            }
        }
        pub use #module_ident::{#queue_ident, #plugin_ident};
    };
    expanded.into()
}
