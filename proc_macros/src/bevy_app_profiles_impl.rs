//! Code used by `bevy_app_profiles!`.
//!
//! One macro call creates the Godot entry point and every profile node exposed
//! by a game library.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    Ident, Path, Result, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

struct ProfileSpec {
    node_name: Ident,
    configure_path: Path,
}

impl Parse for ProfileSpec {
    fn parse(input: ParseStream) -> Result<Self> {
        let node_name = input.parse()?;
        input.parse::<Token![=>]>()?;
        let configure_path = input.parse()?;

        Ok(Self {
            node_name,
            configure_path,
        })
    }
}

struct ProfileList {
    profile_list: Punctuated<ProfileSpec, Token![,]>,
}

impl Parse for ProfileList {
    fn parse(input: ParseStream) -> Result<Self> {
        let profile_list = Punctuated::parse_terminated(input)?;
        if profile_list.is_empty() {
            return Err(input.error("add at least one Bevy app profile"));
        }

        Ok(Self { profile_list })
    }
}

pub fn expand(input: TokenStream) -> TokenStream {
    let spec = syn::parse_macro_input!(input as ProfileList);
    expand_profile_list(spec).into()
}

fn expand_profile_list(spec: ProfileList) -> TokenStream2 {
    let profile_node_list = spec.profile_list.iter().map(|profile| {
        let node_name = &profile.node_name;
        let configure_path = &profile.configure_path;

        quote! {
            /// Selects one fixed Bevy setup for the referenced [`BevyApp`].
            ///
            /// Add this node in an authored Godot scene and assign its
            /// `bevy_app` property to the child that hosts Bevy. Other bridge
            /// nodes still resolve that child through `BevyApp::resolve()`.
            #[derive(GodotClass)]
            #[class(base=Node)]
            pub struct #node_name {
                #[export]
                bevy_app: Option<Gd<BevyApp>>,
                #[base]
                base: Base<Node>,
            }

            #[godot_api]
            impl INode for #node_name {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        bevy_app: None,
                        base,
                    }
                }

                fn enter_tree(&mut self) {
                    if Engine::singleton().is_editor_hint() {
                        return;
                    }

                    let Some(mut bevy_app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!(
                            "{}: assign its BevyApp property in the Inspector",
                            stringify!(#node_name),
                        );
                        return;
                    };

                    bevy_app
                        .bind_mut()
                        .set_app_builder(
                            super::#configure_path as fn(&mut App),
                            stringify!(#node_name),
                        );
                }
            }
        }
    });
    let module_name = format_ident!("__bevy_app_profiles");

    quote! {
        #[allow(non_snake_case)]
        mod #module_name {
            use bevy::app::App;
            use godot::classes::{Engine, INode, Node};
            use godot::global::godot_error;
            use godot::init::ExtensionLibrary;
            use godot::obj::{Base, Gd};
            use godot::prelude::*;

            use bevy_godot4::prelude::BevyApp;

            struct BevyAppProfilesExtensionLibrary;

            #[gdextension]
            unsafe impl ExtensionLibrary for BevyAppProfilesExtensionLibrary {}

            #(#profile_node_list)*
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ProfileList, expand_profile_list};
    use syn::parse_quote;

    #[test]
    fn profiles_macro_generates_one_extension_and_every_profile_node() {
        let output = expand_profile_list(parse_quote! {
            GameplayBevyApp => gameplay::configure_gameplay,
            BenchmarkBevyApp => benchmark::configure,
        })
        .to_string();

        assert!(output.contains("BevyAppProfilesExtensionLibrary"));
        assert_eq!(output.matches("unsafe impl ExtensionLibrary").count(), 1);
        assert!(output.contains("struct GameplayBevyApp"));
        assert!(output.contains("struct BenchmarkBevyApp"));
        assert!(output.contains("set_app_builder"));
    }

    #[test]
    fn profiles_macro_rejects_an_empty_profile_list() {
        assert!(syn::parse_str::<ProfileList>("").is_err());
    }
}
