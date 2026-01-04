//! proc-macro: import_queue!{ config: SomeTransferConfig, ... }
//!
//! This replaces the old `#[derive(ImportQueue)]` approach.
//!
//! Usage:
//! import_queue! {
//!     config: InitializeEntityIntentionTransferConfig,
//! }
//!
//! Where `InitializeEntityIntentionTransferConfig: DataTransferConfig` and
//! `DataType` is your Bevy `Message` type (the intention), and `DtoType` is the Godot DTO.

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

    let cfg_path: Path = spec.config;
    let (domain_ident, domain_snake) = derive_domain_from_config(&cfg_path).unwrap();

    // Public API idents
    let queue_rc_ident = format_ident!("{}Queue", domain_ident); // <Domain>Queue
    let plugin_ident = format_ident!("{}ImportPlugin", domain_ident); // <Domain>ImportPlugin

    // Internal names
    let queue_storage_ident = format_ident!("__{}_ImportQueueStorage", domain_ident);
    let drain_fn_ident = format_ident!("__drain_{}", domain_snake);
    let module_ident = format_ident!("__{}_import_queue", domain_snake);

    // NOTE: We do NOT generate a Godot-visible enqueue signature based on DTO fields
    // because DTO may contain Godot-only types. We enqueue DTO itself and convert
    // to Message in the drain system via DataTransferConfig::from_dto().
    //
    // Godot side call is: queue.enqueue(dto: Gd<DtoType>)
    //
    // This matches your requirement after the Vec3 issue.

    let expanded: TokenStream2 = quote! {
        #[allow(non_snake_case)]
        mod #module_ident {
            use bevy::prelude::*;
            use std::collections::VecDeque;

            use godot::prelude::*;
            use godot::classes::Node;
            use godot::obj::Base;
            use godot::global::godot_error;

            // IMPORTANT:
            // - BevyApp is in bevy_godot4 crate root (per your fix), not necessarily in prelude.
            use bevy_godot4::BevyApp;
            use bevy_godot4::DataTransferConfig;

            // -----------------------------------------
            // Types derived from config
            // -----------------------------------------
            type __Cfg = super::#cfg_path;
            type __Msg = <__Cfg as DataTransferConfig>::DataType;
            type __Dto = <__Cfg as DataTransferConfig>::DtoType;

            // -----------------------------------------
            // Per-app NonSend queue storage (DTOs)
            // -----------------------------------------
            #[derive(Default, Debug)]
            struct #queue_storage_ident {
                q: VecDeque<Gd<__Dto>>,
            }

            impl #queue_storage_ident {
                fn push(&mut self, dto: Gd<__Dto>) {
                    self.q.push_back(dto);
                }

                fn drain_into(&mut self, out: &mut MessageWriter<__Msg>) {
                    while let Some(dto) = self.q.pop_front() {
                        // DTO -> Message conversion happens here
                        let msg: __Msg = <__Cfg as DataTransferConfig>::from_dto(&dto);
                        out.write(msg);
                    }
                }
            }

            // -----------------------------------------
            // RefCounted Queue Accessor (composable)
            // -----------------------------------------
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #queue_rc_ident {
                bevy_app: Option<Gd<BevyApp>>,
                #[base]
                base: Base<RefCounted>,
            }

            #[godot_api]
            impl #queue_rc_ident {
                /// Attach this queue accessor to a host Node (used to locate parent BevyApp).
                ///
                /// Cache lookup result. Call once from some Node's _ready().
                #[func]
                pub fn bind_bevy_app(&mut self, app: Gd<bevy_godot4::BevyApp>) {
                    self.bevy_app = Some(app);
                }

                /// Enqueue a DTO. Conversion to Bevy message happens in the drain system.
                #[func]
                pub fn enqueue(&mut self, dto: Gd<__Dto>) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!(
                            "[ImportQueue:{}] enqueue() called before bind_bevy_app() (or BevyApp not found)",
                            stringify!(#cfg_path),
                        );
                        return;
                    };

                    // Write into per-app non-send queue
                    app.bind_mut().with_world_mut(|world: &mut World| {
                        if world.get_non_send_resource::<#queue_storage_ident>().is_none() {
                            world.insert_non_send_resource::<#queue_storage_ident>(#queue_storage_ident::default());
                        }
                        let mut q = world.non_send_resource_mut::<#queue_storage_ident>();
                        q.push(dto);
                    });
                }

                #[func]
                pub fn is_attached(&self) -> bool {
                    self.bevy_app.is_some()
                }
            }

            // -----------------------------------------
            // Drain system (Bevy-side)
            // -----------------------------------------
            fn #drain_fn_ident(
                mut out: MessageWriter<__Msg>,
                mut q: NonSendMut<#queue_storage_ident>,
            )
            where
                __Cfg: DataTransferConfig,
                __Msg: Message + Default + Sized,
                __Dto: GodotClass + godot::obj::NewGd,
            {
                q.drain_into(&mut out);
            }

            // -----------------------------------------
            // Plugin
            // -----------------------------------------
            pub struct #plugin_ident;
            impl Plugin for #plugin_ident {
                fn build(&self, app: &mut App) {
                    // compile-time “config implements DataTransferConfig”
                    // plus sanity bounds
                    fn __assert_bounds<C>()
                    where
                        C: DataTransferConfig,
                        <C as DataTransferConfig>::DataType: Message + Default + Sized,
                        <C as DataTransferConfig>::DtoType: GodotClass + godot::obj::NewGd,
                    {}
                    __assert_bounds::<__Cfg>();

                    app.add_message::<__Msg>()
                        .init_non_send_resource::<#queue_storage_ident>()
                        .add_systems(FixedPreUpdate, #drain_fn_ident);
                }
            }
        }

        pub use #module_ident::{ #queue_rc_ident, #plugin_ident };
    };

    expanded.into()
}
