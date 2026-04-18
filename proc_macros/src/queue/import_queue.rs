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
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, Path, Result, Token,
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
    let queue_write_subsystem_ident = format_ident!("__{}_ImportQueueWriteSubsystem", domain_ident);
    let queue_read_subsystem_ident = format_ident!("__{}_ImportQueueReadSubsystem", domain_ident);
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
            use bevy::ecs::system::SystemParam;
            use std::collections::VecDeque;

            use godot::prelude::*;
            use godot::classes::Node;
            use godot::obj::Base;
            use godot::global::godot_error;

            // IMPORTANT:
            // - BevyApp is in bevy_godot4 crate root (per your fix), not necessarily in prelude.
            use bevy_godot4::prelude::BevyApp;
            use bevy_godot4::prelude::DataTransferConfig;
            use bevy_godot4::prelude::IdentitySubsystem;

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

                fn drain_into(
                    &mut self,
                    out: &mut MessageWriter<__Msg>,
                    identity: &mut IdentitySubsystem,
                ) {
                    while let Some(dto) = self.q.pop_front() {
                        // DTO -> Message conversion happens here
                        let msg: __Msg = <__Cfg as DataTransferConfig>::from_dto(&dto, identity);
                        out.write(msg);
                    }
                }
            }

            #[derive(SystemParam)]
            struct #queue_write_subsystem_ident<'w, 's> {
                queue: NonSendMut<'w, #queue_storage_ident>,
                phantom: std::marker::PhantomData<&'s ()>,
            }

            impl #queue_write_subsystem_ident<'_, '_> {
                fn push(&mut self, dto: Gd<__Dto>) {
                    self.queue.push(dto);
                }

                fn push_batch(&mut self, dtos: Vec<Gd<__Dto>>) {
                    for dto in dtos {
                        self.queue.push(dto);
                    }
                }
            }

            #[derive(SystemParam)]
            struct #queue_read_subsystem_ident<'w, 's> {
                queue: NonSendMut<'w, #queue_storage_ident>,
                phantom: std::marker::PhantomData<&'s ()>,
            }

            impl #queue_read_subsystem_ident<'_, '_> {
                fn drain_into(
                    &mut self,
                    out: &mut MessageWriter<__Msg>,
                    identity: &mut IdentitySubsystem,
                ) {
                    self.queue.drain_into(out, identity);
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
                pub fn bind_bevy_app(&mut self, app: Gd<BevyApp>) {
                    self.bevy_app = Some(app);
                }

                /// Enqueue a DTO. Conversion to Bevy message happens in the drain system.
                #[func]
                pub fn enqueue(&mut self, dto: Gd<__Dto>) {
                    self.enqueue_many(vec![dto]);
                }

                pub fn enqueue_many(&mut self, dtos: Vec<Gd<__Dto>>) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!(
                            "[ImportQueue:{}] enqueue() called before bind_bevy_app() (or BevyApp not found)",
                            stringify!(#cfg_path),
                        );
                        return;
                    };

                    // Write into per-app queue via subsystem API
                    app.bind_mut().with_world_mut(|world: &mut World| {
                        if world.get_non_send_resource::<#queue_storage_ident>().is_none() {
                            world.insert_non_send_resource::<#queue_storage_ident>(#queue_storage_ident::default());
                        }

                        let mut state: bevy::ecs::system::SystemState<#queue_write_subsystem_ident<'_, '_>> =
                            bevy::ecs::system::SystemState::new(world);
                        {
                            let mut queue = state.get_mut(world);
                            queue.push_batch(dtos);
                        }
                        state.apply(world);
                    });
                }

                #[func]
                pub fn enqueue_batch(&mut self, dtos: Array<Gd<__Dto>>) {
                    let mut batch: Vec<Gd<__Dto>> = Vec::with_capacity(dtos.len() as usize);
                    for i in 0..dtos.len() {
                        if let Some(dto) = dtos.get(i) {
                            batch.push(dto);
                        }
                    }
                    self.enqueue_many(batch);
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
                mut q: #queue_read_subsystem_ident,
                mut identity: IdentitySubsystem,
            )
            where
                __Cfg: DataTransferConfig,
                __Msg: Message + Default + Sized,
                __Dto: GodotClass + godot::obj::NewGd,
            {
                q.drain_into(&mut out, &mut identity);
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
