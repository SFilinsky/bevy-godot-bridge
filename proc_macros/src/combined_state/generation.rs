use super::declaration::{Field, Source, Spec};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Expr, Ident, LitStr, Path, Type, parse_quote};

pub(super) fn generate(spec: Spec) -> TokenStream {
    let (config, name, definition) = match spec.source {
        Source::Existing { config, name } => (config, name, TokenStream::new()),
        Source::Declared {
            prefix,
            input_list,
            mapping,
            label,
        } => {
            let config = format_ident!("{prefix}CombinedStateConfig");
            let name = format_ident!("{prefix}CombinedStateNode");
            let values = format_ident!("{prefix}Values");
            let dto = format_ident!("{prefix}CombinedDto");
            let label = label
                .unwrap_or_else(|| LitStr::new(&prefix.to_string().to_snake_case(), prefix.span()));
            let definition = define_values(&config, &values, &dto, &input_list, mapping, &label);
            (parse_quote!(#config), name, definition)
        }
    };
    let registration = register_source(&config, &name);
    quote! { #definition #registration }
}

fn is_optional(ty: &Type) -> bool {
    matches!(ty, Type::Path(path) if path.path.segments.last().is_some_and(|part| part.ident == "Option"))
}

fn define_values(
    config: &Ident,
    values: &Ident,
    dto: &Ident,
    input_list: &[Field],
    mapping: Option<(Vec<Field>, Expr)>,
    label: &LitStr,
) -> TokenStream {
    let mapping_fn = Ident::new("__state_value_mapping_fn", Span::mixed_site());
    let inputs = format_ident!("{config}Inputs");
    let input = format_ident!("{config}Input");
    let config_trait = format_ident!("{config}Trait");
    let value_trait = format_ident!("{config}ValueTrait");
    let payload_trait = format_ident!("{config}PayloadTrait");
    let mapping_type = format_ident!("{config}MappingFn");
    let array = format_ident!("{config}Array");
    let variant = format_ident!("{config}Variant");
    let convert = format_ident!("{config}GodotConvert");
    let to_godot = format_ident!("{config}ToGodot");
    let by_value = format_ident!("{config}ByValue");
    let gd = format_ident!("{config}Gd");
    let base = format_ident!("{config}Base");
    let refcounted = format_ident!("{config}RefCounted");
    let class = format_ident!("{config}Class");
    let input_names: Vec<_> = input_list.iter().map(|field| &field.name).collect();
    let input_types: Vec<_> = input_list.iter().map(|field| &field.ty).collect();

    // One declaration defines both native fields and named Godot fields for synchronous reads.
    let (output_list, godot_types, conversions, map_body) = if let Some((outputs, mapping)) =
        mapping
    {
        let types: Vec<TokenStream> = outputs
            .iter()
            .map(|field| {
                let ty = &field.ty;
                if is_optional(ty) {
                    quote!(#variant)
                } else {
                    quote!(<#ty as #convert>::Via)
                }
            })
            .collect();
        let conversions: Vec<TokenStream> = outputs.iter().map(|field| {
            let name=&field.name;
            if is_optional(&field.ty) {
                quote!(match &self.#name { Some(value) => value.to_variant(), None => #variant::nil() })
            } else { quote!(self.#name.to_godot_owned()) }
        }).collect();
        let body = quote! {
            let #mapping_fn: fn(#(<#input_types as #inputs>::Values),*) -> Option<#values> = #mapping;
            #mapping_fn(#(#input_names),*)
        };
        (outputs, types, conversions, body)
    } else {
        let outputs = input_list
            .iter()
            .map(|field| {
                let ty = &field.ty;
                Field {
                    name: field.name.clone(),
                    ty: parse_quote!(<#ty as #inputs>::Values),
                }
            })
            .collect();
        let types = input_types
            .iter()
            .map(|ty| quote!(<#ty as #input>::GodotValue))
            .collect();
        let conversions = input_list
            .iter()
            .map(|field| {
                let name = &field.name;
                let ty = &field.ty;
                quote!(<#ty as #input>::godot_value(&self.#name))
            })
            .collect();
        (
            outputs,
            types,
            conversions,
            quote!(Some(#values { #(#input_names),* })),
        )
    };
    let names: Vec<_> = output_list.iter().map(|field| &field.name).collect();
    let types: Vec<_> = output_list.iter().map(|field| &field.ty).collect();
    quote! {
        use bevy_godot4::prelude::{StateInputs as #inputs, StateInput as #input, CombinedStateConfig as #config_trait,
            ExportValue as #value_trait, UpdatePayload as #payload_trait, StateMappingFn as #mapping_type};
        use godot::builtin::{Array as #array, Variant as #variant};
        use godot::meta::{GodotConvert as #convert, ToGodot as #to_godot, ByValue as #by_value};
        use godot::obj::{Gd as #gd, Base as #base};
        use godot::classes::RefCounted as #refcounted;
        use godot::prelude::GodotClass as #class;
        #[derive(Clone)]
        struct #values { #(#names: #types,)* }
        #[derive(#class)]
        #[class(base=RefCounted, no_init)]
        struct #dto { #(#[var(get)] #names: #godot_types,)* #[base] base: #base<#refcounted> }
        impl #convert for #values { type Via = #gd<#dto>; }
        impl #to_godot for #values {
            type Pass = #by_value;
            fn to_godot(&self) -> Self::Via {
                #gd::from_init_fn(|base| #dto { base, #(#names: #conversions,)* })
            }
        }
        impl #value_trait for #values { const NAME: &'static str = #label; }
        impl #payload_trait for #values {
            type Fields = (#(#types,)*);
            fn into_fields(self) -> Self::Fields { (#(self.#names,)*) }
            fn write_arguments(&self, arguments: &mut #array<#variant>) {
                arguments.clear();
                #(arguments.push(&(#conversions).to_variant());)*
            }
        }
        struct #config;
        impl #config_trait for #config {
            type Inputs = (#(#input_types,)*);
            type Value = #values;
            const VALUE_MAPPING_FN: #mapping_type<Self> = |(#(#input_names,)*)| { #map_body };
        }
    }
}

fn register_source(config: &Path, name: &Ident) -> TokenStream {
    let module = format_ident!("{}_api", name.to_string().to_snake_case());
    let config_path = if config.leading_colon.is_some()
        || config
            .segments
            .first()
            .is_some_and(|part| part.ident == "crate")
    {
        quote!(#config)
    } else {
        quote!(super::#config)
    };
    let config: Path = parse_quote!(Config);
    quote! {
        mod #module {
            type Config = #config_path;
            use godot::obj::Gd;
            use godot::classes::Node;
            use godot::meta::{GodotConvert, ToGodot};
            use godot::prelude::{GodotClass, godot_api};
            use bevy_godot4::prelude::{CombinedStateConfig, EntityMeta, UpdateReceiver, UpdateSubscription,
                read_combined_state, report_state_error, subscribe_source, subscribe_source_id};
            type Value = <#config as CombinedStateConfig>::Value;
            type Dto = <Value as GodotConvert>::Via;
            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #name;
            #[godot_api]
            impl #name {
                #[func]
                fn implements(host: Gd<Node>, id: i64) -> bool { read_combined_state::<#config>(&host,id).is_ok() }
                #[func]
                fn get_current(host: Gd<Node>, id: i64) -> Option<Dto> {
                    match read_combined_state::<#config>(&host,id) {
                        Ok(value) => Some(value.to_godot_owned()),
                        Err(error) => { report_state_error(stringify!(#name),id,&error); None }
                    }
                }
                #[func]
                fn subscribe(host: Gd<Node>, entity: Gd<EntityMeta>, receiver: Gd<UpdateReceiver>) -> Option<Gd<UpdateSubscription>> {
                    subscribe_source::<#config>(host,entity,receiver)
                }
                #[func]
                fn subscribe_id(host: Gd<Node>, id: i64, receiver: Gd<UpdateReceiver>) -> Option<Gd<UpdateSubscription>> {
                    subscribe_source_id::<#config>(host,id,receiver)
                }
            }
        }
        pub use #module::#name;
    }
}

#[cfg(test)]
mod tests {
    use super::generate;
    use crate::combined_state::declaration::Spec;
    use syn::{File, parse_str, parse2};

    #[test]
    fn forwarding_and_mapped_readers_generate_valid_rust_items() {
        for source in [
            "prefix: Animation, inputs: { attack: Optional<Attack>, movement: Optional<Movement> }",
            "prefix: Health, inputs: { health: Required<Health> }, outputs: { current: f32 }, value_mapping_fn: |health| Some(HealthValues { current: health.current })",
            "prefix: Health, inputs: { health: Required<Health> }, outputs: { current: f32 }, value_mapping_fn: map_health",
            "config: CustomConfig, name: CustomUpdates",
        ] {
            let spec = parse_str::<Spec>(source).unwrap();
            parse2::<File>(generate(spec)).expect("generated declarations must form valid Rust");
        }
    }

    #[test]
    fn default_labels_keep_word_boundaries_and_acronyms() {
        for (prefix, label) in [
            ("BuildingView", "building_view"),
            ("HTMLInput", "html_input"),
        ] {
            let source = format!("prefix: {prefix}, inputs: {{ value: Required<Config> }}");
            let generated = generate(parse_str::<Spec>(&source).unwrap()).to_string();
            assert!(generated.contains(&format!("\"{label}\"")));
        }
    }
}
