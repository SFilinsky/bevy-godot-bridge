//! proc-macro: settings_pipeline!{ config: SomeTransferConfig }
//!
//! Generates a singleton Godot initialization node that sends one settings DTO into Bevy.

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, Path, Result, Token,
};

const CFG_SUFFIX: &str = "TransferConfig";

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
        Ok(Self { config })
    }
}

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
            format!("config type must end with `{CFG_SUFFIX}`"),
        ));
    }

    let domain_name = cfg_name.strip_suffix(CFG_SUFFIX).unwrap().to_string();
    if domain_name.is_empty() {
        return Err(syn::Error::new(
            cfg_ident.span(),
            format!("config type name cannot be just `{CFG_SUFFIX}`"),
        ));
    }

    let domain_ident = Ident::new(&domain_name, Span::call_site());
    let domain_snake = to_snake(&domain_name);

    Ok((domain_ident, domain_snake))
}

pub fn expand(input: TokenStream) -> TokenStream {
    let spec = parse_macro_input!(input as Spec);

    let cfg_path = spec.config;
    let (domain_ident, domain_snake) = derive_domain_from_config(&cfg_path).unwrap();

    let queue_accessor_ident = format_ident!("{}Queue", domain_ident);
    let queue_plugin_ident = format_ident!("{}ImportPlugin", domain_ident);
    let node_ident = format_ident!("{}InitializationNode", domain_ident);
    let plugin_ident = format_ident!("{}PipelinePlugin", domain_ident);
    let module_ident = format_ident!("{}_settings_pipeline", domain_snake);

    let expanded: TokenStream2 = quote! {
        pub mod #module_ident {
            use super::*;
            use bevy::prelude::*;
            use godot::global::{godot_error, godot_warn};
            use godot::obj::Base;
            use godot::prelude::*;
            use bevy_godot4::prelude::*;

            type SettingsTransferConfig = #cfg_path;
            type SettingsMessage = <SettingsTransferConfig as DataTransferConfig>::DataType;
            type SettingsDto = <SettingsTransferConfig as DataTransferConfig>::DtoType;

            bevy_godot4::prelude::import_queue! {
                config: #cfg_path,
            }

            #[derive(Resource, Default)]
            struct SettingsImportState {
                did_receive_settings: bool,
            }

            fn enforce_singleton_settings(
                mut message_reader: MessageReader<SettingsMessage>,
                mut state: ResMut<SettingsImportState>,
            )
            where
                SettingsMessage: Message + Default + Clone + Sized,
            {
                for _message in message_reader.read() {
                    if !state.did_receive_settings {
                        state.did_receive_settings = true;
                        continue;
                    }

                    if cfg!(debug_assertions) {
                        panic!(
                            "[{}] received more than one settings initialization message",
                            stringify!(#node_ident)
                        );
                    }

                    godot_warn!(
                        "[{}] received more than one settings initialization message; settings handlers should keep the first value",
                        stringify!(#node_ident)
                    );
                }
            }

            #[derive(GodotClass)]
            #[class(base=Node)]
            pub struct #node_ident {
                settings_cache: Option<Gd<SettingsDto>>,
                queue: Gd<#queue_accessor_ident>,
                did_enqueue: bool,

                #[base]
                base: Base<Node>,
            }

            #[godot_api]
            impl #node_ident {
                fn enqueue_settings(&mut self) {
                    if self.did_enqueue {
                        if cfg!(debug_assertions) {
                            panic!(
                                "[{}] settings were enqueued more than once",
                                stringify!(#node_ident)
                            );
                        }

                        godot_warn!(
                            "[{}] settings were enqueued more than once; ignoring later enqueue",
                            stringify!(#node_ident)
                        );
                        return;
                    }

                    let settings = self.settings_cache.clone().unwrap_or_else(|| {
                        <SettingsDto as godot::obj::NewGd>::new_gd()
                    });

                    self.queue.bind_mut().enqueue(settings);
                    self.queue.bind_mut().flush();
                    self.did_enqueue = true;
                }

                #[func]
                pub fn initialize(&mut self) {
                    self.enqueue_settings();
                }

                #[func]
                fn set_settings(&mut self, settings: Gd<SettingsDto>) {
                    self.settings_cache = Some(settings);
                }

                #[func]
                fn get_settings(&self) -> Option<Gd<SettingsDto>> {
                    self.settings_cache.clone()
                }

                #[func]
                fn resolve(owner: Gd<Node>) -> Option<Gd<#node_ident>> {
                    if !owner.is_instance_valid() {
                        return None;
                    }

                    let Ok(app) = BevyApp::resolve(&owner) else {
                        return None;
                    };

                    let mut found = collect_children::<#node_ident>(app.upcast::<Node>(), false);

                    if found.len() > 1 {
                        panic!(
                            "Multiple {} nodes found under the owner node; expected exactly one settings initializer",
                            stringify!(#node_ident)
                        );
                    }

                    found.pop()
                }

            }

            #[godot_api]
            impl INode for #node_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        settings_cache: Some(<SettingsDto as godot::obj::NewGd>::new_gd()),
                        queue: #queue_accessor_ident::new_gd(),
                        did_enqueue: false,
                        base,
                    }
                }

                fn enter_tree(&mut self) {
                    let host_node = &self.base().clone().upcast::<Node>();
                    match BevyApp::resolve(host_node) {
                        Ok(bevy_app) => {
                            self.queue.bind_mut().bind_bevy_app(bevy_app);
                        }
                        Err(err) => {
                            godot_error!(
                                "[{}] failed to bind BevyApp: {}",
                                stringify!(#node_ident),
                                err
                            );
                        }
                    }
                }

                fn ready(&mut self) {
                    InitializationCoordinator::register_initializer_node(
                        self.base().clone().upcast(),
                        InitializationPhase::Configuration,
                    );
                }

                fn process(&mut self, _delta: f64) {
                    self.queue.bind_mut().flush();
                }
            }

            pub struct #plugin_ident;

            impl Plugin for #plugin_ident {
                fn build(&self, app: &mut App) {
                    app.add_plugins(#queue_plugin_ident)
                        .init_resource::<SettingsImportState>()
                        .add_systems(FixedPreUpdate, enforce_singleton_settings);
                }
            }
        }

        pub use #module_ident::{#node_ident, #plugin_ident};
    };

    expanded.into()
}
