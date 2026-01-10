use heck::{ToSnakeCase, ToUpperCamelCase};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    Ident, LitStr, Path, Result, Token, bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// Usage:
/// import_bundle! {
///   name: "Barracks",
///   message: SpawnBarracks,
///   components: [ TransformTransferConfig, FactionTransferConfig ],
/// }
struct Spec {
    name: LitStr,
    message: Path,
    components: Vec<Path>,
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name: Option<LitStr> = None;
        let mut message: Option<Path> = None;
        let mut components: Option<Vec<Path>> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "name" => name = Some(input.parse()?),
                "message" => message = Some(input.parse()?),
                "components" => {
                    let content;
                    bracketed!(content in input);
                    let list = content
                        .parse_terminated(Path::parse, Token![,])?
                        .into_iter()
                        .collect::<Vec<_>>();
                    components = Some(list);
                }
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("Unknown key `{other}`; expected `name`, `message`, `components`"),
                    ));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        let name = name.ok_or_else(|| {
            syn::Error::new(Span::call_site(), "import_bundle!: missing `name: \"...\"`")
        })?;

        let message = message.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "import_bundle!: missing `message: SomeMessageType`",
            )
        })?;

        let components = components.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "import_bundle!: missing `components: [ ... ]`",
            )
        })?;

        Ok(Self {
            name,
            message,
            components,
        })
    }
}

fn last_path_ident(path: &Path) -> Ident {
    path.segments
        .last()
        .expect("path must have at least one segment")
        .ident
        .clone()
}

/// Turn `FooTransferConfig` into `foo` (field name).
fn field_name_from_transfer_config(config_ident: &Ident) -> String {
    let name = config_ident.to_string();
    let base = name.strip_suffix("TransferConfig").unwrap_or(name.as_str());
    base.to_snake_case()
}

pub fn expand(input: TokenStream) -> TokenStream {
    let Spec {
        name,
        message,
        components,
    } = parse_macro_input!(input as Spec);

    let entity_name = name.value();
    let entity_name_camel = entity_name.to_upper_camel_case();
    let entity_name_snake = entity_name.to_snake_case();

    // Module name (one per import bundle)
    let module_ident = format_ident!("{}_import_bundle", entity_name_snake);

    // Gameplay message type (owned by gameplay code)
    let message_type_path = message.clone();
    let message_type_ident = last_path_ident(&message_type_path);

    // Transfer config for the message DTO wrapper (generated here)
    // Must end with TransferConfig to satisfy import_queue! expectations.
    let message_transfer_config_ident = format_ident!("{}TransferConfig", message_type_ident);

    // import_queue! generated types (derived from TransferConfig name)
    //   SpawnBarracksTransferConfig -> SpawnBarracksQueue / SpawnBarracksImportPlugin
    let queue_accessor_ident = format_ident!("{}Queue", message_type_ident);
    let queue_plugin_ident = format_ident!("{}ImportPlugin", message_type_ident);

    // Convenience plugin exported by this macro (named by entity name, not message)
    let bundle_plugin_ident = format_ident!("{}ImportBundlePlugin", entity_name_camel);

    // Godot node that exists in scenes (named by entity name)
    let initialization_node_ident = format_ident!("{}InitializationNode", entity_name_camel);

    // DTO wrapper name (scoped within module, so collisions are unlikely)
    let message_dto_ident = format_ident!("{}Dto", message_type_ident);

    // Field names derived from TransferConfig names
    let mut message_field_idents: Vec<Ident> = Vec::new();
    let mut gatherer_path_field_idents: Vec<Ident> = Vec::new();

    for transfer_config_path in &components {
        let transfer_config_ident = last_path_ident(transfer_config_path);
        let field_snake = field_name_from_transfer_config(&transfer_config_ident);

        message_field_idents.push(Ident::new(&field_snake, Span::call_site()));
        gatherer_path_field_idents.push(Ident::new(
            &format!("{field_snake}_gatherer"),
            Span::call_site(),
        ));
    }

    // DTO fields: <field>: Gd<<Cfg as DataTransferConfig>::DtoType>
    let dto_fields: Vec<TokenStream2> = components
        .iter()
        .zip(message_field_idents.iter())
        .map(|(transfer_config_path, message_field_ident)| {
            quote! {
                #[var]
                pub #message_field_ident: godot::prelude::Gd<<#transfer_config_path as DataTransferConfig>::DtoType>
            }
        })
        .collect();

    // Build component DTOs using gatherers
    let build_component_dtos: Vec<TokenStream2> = components
        .iter()
        .zip(message_field_idents.iter())
        .zip(gatherer_path_field_idents.iter())
        .map(|((transfer_config_path, message_field_ident), gatherer_path_field_ident)| {
            quote! {
                let #message_field_ident: godot::prelude::Gd<<#transfer_config_path as DataTransferConfig>::DtoType> = {
                    type DtoType = <#transfer_config_path as DataTransferConfig>::DtoType;
                    type GathererType = <DtoType as WithGatherer>::Gatherer;

                    // Compile-time: GathererType must implement BuildsDto<DtoType>
                    fn assert_gatherer_builds_dto<G, D>()
                    where
                        D: godot::prelude::GodotClass,
                        G: BuildsDto<D>,
                    {}
                    assert_gatherer_builds_dto::<GathererType, DtoType>();

                    let gatherer_path = self.#gatherer_path_field_ident.clone();
                    if gatherer_path.is_empty() {
                        panic!(
                            concat!(
                                "[", #entity_name, "InitializationNode] Missing gatherer path for: ",
                                stringify!(#message_field_ident)
                            )
                        );
                    }

                    let gatherer_node = self.base().get_node_or_null(&gatherer_path).unwrap_or_else(|| {
                        panic!(
                            concat!(
                                "[", #entity_name, "InitializationNode] Gatherer not found at path '{}' for: ",
                                stringify!(#message_field_ident)
                            ),
                            gatherer_path.to_string()
                        )
                    });

                    let gatherer = gatherer_node.try_cast::<GathererType>().unwrap_or_else(|_| {
                        panic!(
                            concat!(
                                "[", #entity_name, "InitializationNode] Gatherer at '{}' has wrong type for: ",
                                stringify!(#message_field_ident)
                            ),
                            gatherer_path.to_string()
                        )
                    });

                    BuildsDto::<DtoType>::build_dto(&*gatherer.bind())
                };
            }
        })
        .collect();

    // Assign component DTOs into message DTO wrapper
    let assign_component_dtos: Vec<TokenStream2> = message_field_idents
        .iter()
        .map(|message_field_ident| {
            quote! {
                dto_wrapper_bind.#message_field_ident = #message_field_ident;
            }
        })
        .collect();

    // Exposed NodePath fields for gatherers
    let gatherer_nodepath_vars: Vec<TokenStream2> = gatherer_path_field_idents
        .iter()
        .map(|gatherer_path_field_ident| {
            quote! {
                #[export]
                #[var]
                pub #gatherer_path_field_ident: godot::builtin::NodePath
            }
        })
        .collect();

    // Message field presence + type checks.
    //
    // These checks deliberately “touch” the message fields so:
    // - missing field -> error: no field `...` on type `SpawnBarracks`
    // - type mismatch -> error points at expected type (`<Cfg as DataTransferConfig>::DataType`)
    //
    // We do NOT require the message to cover all possible fields; only the ones listed in components.
    let message_field_type_checks: Vec<TokenStream2> = components
        .iter()
        .zip(message_field_idents.iter())
        .map(|(transfer_config_path, message_field_ident)| {
            quote! {
                let _: &mut <#transfer_config_path as DataTransferConfig>::DataType = &mut message_instance.#message_field_ident;
            }
        })
        .collect();

    // DataTransferConfig::update_data mapping:
    // For each field:
    //   <Cfg as DataTransferConfig>::update_data(&dto.field, &mut message.field)
    let transfer_update_message_fields: Vec<TokenStream2> = components
        .iter()
        .zip(message_field_idents.iter())
        .map(|(transfer_config_path, message_field_ident)| {
            quote! {
                <#transfer_config_path as DataTransferConfig>::update_data(
                    &dto_wrapper_bind.#message_field_ident,
                    &mut message_instance.#message_field_ident,
                );
            }
        })
        .collect();

    // DataTransferConfig::update_dto mapping (useful if you ever want Bevy->Godot for debugging / preview)
    let transfer_update_dto_fields: Vec<TokenStream2> = components
        .iter()
        .zip(message_field_idents.iter())
        .map(|(transfer_config_path, message_field_ident)| {
            quote! {
                <#transfer_config_path as DataTransferConfig>::update_dto(
                    &mut dto_wrapper_bind.#message_field_ident,
                    &message_instance.#message_field_ident,
                );
            }
        })
        .collect();

    let expanded: TokenStream2 = quote! {
        pub mod #module_ident {
            use super::*;
            use bevy::prelude::*;
            use godot::prelude::*;
            use bevy_godot4::prelude::*;

            // -----------------------------------------------------------------
            // Compile-time checks (strong typing)
            // -----------------------------------------------------------------
            #[allow(dead_code)]
            fn __assert_message_bounds() {
                fn assert_bounds<M>()
                where
                    M: bevy::prelude::Message + Default + Clone + Sized,
                {}
                assert_bounds::<#message_type_path>();
            }

            #[allow(dead_code)]
            fn __assert_message_fields_and_types() {
                let mut message_instance: #message_type_path = Default::default();
                #( #message_field_type_checks )*
            }

            // -----------------------------------------------------------------
            // Godot DTO wrapper for the gameplay message
            // -----------------------------------------------------------------
            #[derive(GodotClass, Debug)]
            #[class(init, base=RefCounted)]
            pub struct #message_dto_ident {
                #( #dto_fields, )*

                #[base]
                base: Base<RefCounted>,
            }

            // -----------------------------------------------------------------
            // Transfer config: Godot DTO wrapper <-> gameplay Message
            // -----------------------------------------------------------------
            pub struct #message_transfer_config_ident;

            impl DataTransferConfig for #message_transfer_config_ident {
                type DataType = #message_type_path;
                type DtoType = #message_dto_ident;

                fn update_dto(dto: &mut Gd<Self::DtoType>, data: &Self::DataType) {
                    let mut dto_wrapper_bind = dto.bind_mut();
                    let message_instance: &Self::DataType = data;

                    #( #transfer_update_dto_fields )*
                }

                fn update_data(dto: &Gd<Self::DtoType>, data: &mut Self::DataType) {
                    let dto_wrapper_bind = dto.bind();
                    let message_instance: &mut Self::DataType = data;

                    #( #transfer_update_message_fields )*
                }
            }

            // -----------------------------------------------------------------
            // Queue injection (drains DTO into gameplay Message before update)
            // -----------------------------------------------------------------
            bevy_godot4::prelude::import_queue! {
                config: #message_transfer_config_ident,
            }

            // -----------------------------------------------------------------
            // Gatherer bounds: each component DTO must implement WithGatherer
            // -----------------------------------------------------------------
            #[allow(dead_code)]
            fn __assert_with_gatherer_bounds() {
                fn assert_with_gatherer<T: WithGatherer>() {}
                #( assert_with_gatherer::<<#components as DataTransferConfig>::DtoType>(); )*
            }

            // -----------------------------------------------------------------
            // Scene node: builds DTO wrapper from gatherers and enqueues it
            // -----------------------------------------------------------------
            #[derive(GodotClass)]
            #[class(base=Node)]
            pub struct #initialization_node_ident {
                #( #gatherer_nodepath_vars, )*

                queue: Gd<#queue_accessor_ident>,

                #[export]
                #[var]
                destroy_parent: bool,

                #[base]
                base: Base<Node>,
            }

            #[godot_api]
            impl #initialization_node_ident {
                fn enqueue(&mut self) {
                    #( #build_component_dtos )*

                    let mut dto_wrapper: Gd<#message_dto_ident> = #message_dto_ident::new_gd();
                    {
                        let mut dto_wrapper_bind = dto_wrapper.bind_mut();
                        #( #assign_component_dtos )*
                    }

                    self.queue.bind_mut().enqueue(dto_wrapper);
                }

                #[func]
                pub fn send(&mut self) {
                    self.enqueue();
                }
            }

            #[godot_api]
            impl INode for #initialization_node_ident {
                fn init(base: Base<Node>) -> Self {
                    Self {
                        #( #gatherer_path_field_idents: godot::builtin::NodePath::default(), )*
                        queue: #queue_accessor_ident::new_gd(),
                        destroy_parent: true,
                        base,
                    }
                }

                fn enter_tree(&mut self) {
                    let host_node = &self.base().clone().upcast();
                    let bevy_app = BevyApp::find_for(host_node).expect("BevyApp not found");

                    self.queue
                        .bind_mut()
                        .bind_bevy_app(bevy_app);
                }

                fn ready(&mut self) {
                    self.enqueue();

                    // Destroy placeholder scene object (spawn is driven by Bevy export afterwards).
                    if (!self.destroy_parent) {
                        self.base_mut().queue_free();
                        return;
                    }

                    if let Some(mut parent) = self.base().get_parent() {
                        parent.queue_free();
                    } else {
                        self.base_mut().queue_free();
                    }
                }
            }

            // -----------------------------------------------------------------
            // Convenience plugin: registers the queue drain plugin
            // -----------------------------------------------------------------
            pub struct #bundle_plugin_ident;

            impl Plugin for #bundle_plugin_ident {
                fn build(&self, app: &mut App) {
                    app.add_plugins(#queue_plugin_ident);
                }
            }
        }

        pub use #module_ident::#bundle_plugin_ident;
    };

    expanded.into()
}
