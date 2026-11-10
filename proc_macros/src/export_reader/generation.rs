use super::declaration::{Field, Source, Spec};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Expr, Ident, LitStr, Path, parse_quote};

pub(super) fn generate(spec: Spec) -> TokenStream {
    // Build the config and values only when the caller declares them here.
    let (config, name, definition) = match spec.source {
        Source::Existing { config, name } => (config, name, TokenStream::new()),
        Source::Declared {
            prefix,
            input_list,
            mapping,
            label,
        } => {
            let config = format_ident!("{prefix}ReaderConfig");
            let name = format_ident!("{prefix}ValueUpdates");
            let values = format_ident!("{prefix}Values");
            let label = label
                .unwrap_or_else(|| LitStr::new(&prefix.to_string().to_snake_case(), prefix.span()));
            let definition = define_values(&config, &values, &input_list, mapping, &label);
            (parse_quote!(#config), name, definition)
        }
    };
    // The Godot entry point still uses the shared runtime implementation.
    let registration = register_reader(&config, &name, &spec.methods);
    quote! { #definition #registration }
}

fn define_values(
    config: &Ident,
    values: &Ident,
    input_list: &[Field],
    mapping: Option<(Vec<Field>, Expr)>,
    label: &LitStr,
) -> TokenStream {
    let mapping_fn = Ident::new("__reader_value_mapping_fn", Span::mixed_site());
    let inputs_trait = format_ident!("{config}Inputs");
    let config_trait = format_ident!("{config}Trait");
    let value_trait = format_ident!("{config}ValueTrait");
    let mapping_type = format_ident!("{config}MappingFn");
    let array = format_ident!("{config}Array");
    let variant = format_ident!("{config}Variant");
    let to_godot = format_ident!("{config}ToGodot");
    let input_name_list: Vec<_> = input_list.iter().map(|field| &field.name).collect();
    let input_type_list: Vec<_> = input_list.iter().map(|field| &field.ty).collect();

    // Forwarded field types come from their input declarations; mappings declare output types.
    let (output_list, map_body) = if let Some((output_list, mapping)) = mapping {
        let body = quote! {
            let #mapping_fn: fn(#(<#input_type_list as #inputs_trait>::Values),*) -> Option<#values> = #mapping;
            #mapping_fn(#(#input_name_list),*)
        };
        (output_list, body)
    } else {
        let output_list = input_list
            .iter()
            .map(|field| {
                let ty = &field.ty;
                Field {
                    name: field.name.clone(),
                    ty: parse_quote!(<#ty as #inputs_trait>::Values),
                }
            })
            .collect();
        (
            output_list,
            quote! { Some(#values { #(#input_name_list),* }) },
        )
    };
    let output_name_list: Vec<_> = output_list.iter().map(|field| &field.name).collect();
    let output_type_list: Vec<_> = output_list.iter().map(|field| &field.ty).collect();

    // Keep callback arguments in authored order, including absent optional DTOs.
    quote! {
        use bevy_godot4::prelude::{ReaderInputs as #inputs_trait, ReaderConfig as #config_trait,
            ExportValue as #value_trait, ValueMappingFn as #mapping_type};
        use godot::builtin::{Array as #array, Variant as #variant};
        use godot::meta::ToGodot as #to_godot;

        #[derive(Clone)]
        struct #values { #(#output_name_list: #output_type_list,)* }
        impl #value_trait for #values {
            const NAME: &'static str = #label;
            fn write_arguments(self, argument_list: &mut #array<#variant>) {
                argument_list.clear();
                #(argument_list.push(&self.#output_name_list.to_variant());)*
            }
        }
        struct #config;
        impl #config_trait for #config {
            type Inputs = (#(#input_type_list,)*);
            type Value = #values;
            const VALUE_MAPPING_FN: #mapping_type<Self> = |(#(#input_name_list,)*)| { #map_body };
        }
    }
}

fn register_reader(config: &Path, name: &Ident, methods: &TokenStream) -> TokenStream {
    let gd = format_ident!("{name}Gd");
    let node = format_ident!("{name}Node");
    let meta = format_ident!("{name}Meta");
    let callable = format_ident!("{name}Callable");
    let subscription = format_ident!("{name}Subscription");
    let watch = format_ident!("{name}Watch");
    let derive = format_ident!("{name}Class");
    let api = format_ident!("{name}Api");
    quote! {
        use godot::obj::Gd as #gd;
        use godot::classes::Node as #node;
        use godot::builtin::Callable as #callable;
        use godot::prelude::{GodotClass as #derive, godot_api as #api};
        use bevy_godot4::prelude::{EntityMeta as #meta, ExportValueSubscription as #subscription, watch_reader as #watch};
        #[derive(#derive)]
        #[class(init, base=RefCounted)]
        struct #name;

        #[#api]
        impl #name {
            #methods
            /// Receive prepared values after Bevy returns; cancel when the view leaves.
            #[func]
            fn watch(host: #gd<#node>, entity: #gd<#meta>, callback: #callable) -> #gd<#subscription> {
                #watch::<#config>(host, entity, callback)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::generate;
    use crate::export_reader::declaration::Spec;
    use syn::{File, parse_str, parse2};

    #[test]
    fn forwarding_and_mapped_readers_generate_valid_rust_items() {
        for source in [
            "prefix: Animation, inputs: { attack: Optional<Attack>, movement: Optional<Movement> }",
            "prefix: Health, inputs: { health: Required<Health> }, outputs: { current: f32 }, value_mapping_fn: |health| Some(HealthValues { current: health.current })",
            "prefix: Health, inputs: { health: Required<Health> }, outputs: { current: f32 }, value_mapping_fn: map_health",
            "config: CustomConfig, name: CustomUpdates, methods: { #[func] fn extra() {} }",
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
