//! Generate owned Rust values and typed Godot access from one field declaration.
use proc_macro::TokenStream;
use proc_macro2::TokenStream as Tokens;
use quote::{format_ident, quote};
use syn::{Error, MetaNameValue, Result, parse2};
use syn::{
    Fields, GenericArgument, Ident, ItemStruct, PathArguments, Type,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
};

fn container(ty: &Type) -> Option<(String, &Type)> {
    let Type::Path(path) = ty else {
        return None;
    };
    let part = path.path.segments.last()?;
    let PathArguments::AngleBracketed(arguments) = &part.arguments else {
        return None;
    };
    let GenericArgument::Type(inner) = arguments.args.first()? else {
        return None;
    };
    Some((part.ident.to_string(), inner))
}

fn dto_type(ty: &Type) -> Tokens {
    if let Some((kind, inner)) = container(ty) {
        let inner = dto_type(inner);
        if kind == "Option" {
            return quote!(Option<#inner>);
        }
        if kind == "Vec" {
            return quote!(godot::builtin::Array<#inner>);
        }
    }
    quote!(godot::obj::Gd<<#ty as bevy_godot4::prelude::TransferSnapshot>::Dto>)
}

fn expose(ty: &Type, value: Tokens) -> Tokens {
    if let Some((kind, inner)) = container(ty) {
        let inner = expose(inner, quote!(item));
        if kind == "Option" {
            return quote!((#value).map(|item| #inner));
        }
        if kind == "Vec" {
            return quote!((#value).into_iter().map(|item| #inner).collect());
        }
    }
    quote!(bevy_godot4::prelude::TransferSnapshot::into_dto(#value))
}

fn capture_snapshot(ty: &Type, value: Tokens) -> Tokens {
    if let Some((kind, inner)) = container(ty) {
        let inner = capture_snapshot(inner, quote!(item));
        if kind == "Option" {
            return quote!((#value).as_ref().map(|item| #inner));
        }
        if kind == "Vec" {
            return quote!((#value).iter_shared().map(|item| #inner).collect());
        }
    }
    quote!(bevy_godot4::prelude::TransferDto::capture_snapshot(&*(#value).bind()))
}

fn copy_field(ty: &Type, value: Tokens) -> Tokens {
    let Type::Path(path) = ty else {
        return quote!((#value).clone());
    };
    let name = path.path.segments.last().unwrap().ident.to_string();
    if matches!(
        name.as_str(),
        "Array" | "VarArray" | "Dictionary" | "VarDictionary"
    ) {
        quote!((#value).duplicate_deep())
    } else {
        quote!((#value).clone())
    }
}

struct Options {
    dto: Ident,
}
impl Parse for Options {
    fn parse(input: ParseStream) -> Result<Self> {
        let dto = input.parse()?;
        if !input.is_empty() {
            return Err(input.error("pass only the DTO name; every DTO is RefCounted"));
        }
        Ok(Self { dto })
    }
}

pub fn expand(args: TokenStream, input: TokenStream) -> TokenStream {
    let options = parse_macro_input!(args as Options);
    let value = parse_macro_input!(input as ItemStruct);
    generate(options, value)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn generate(options: Options, mut value: ItemStruct) -> Result<Tokens> {
    let Options { dto } = options;
    // A concrete Godot class needs a concrete field schema.
    if !value.generics.params.is_empty() {
        return Err(Error::new_spanned(
            &value.generics,
            "declare a concrete bridge value without type parameters",
        ));
    }
    let name = &value.ident;
    let visibility = &value.vis;
    let Fields::Named(fields) = &mut value.fields else {
        return Err(Error::new_spanned(&value, "bridge_dto needs named fields"));
    };
    // Keep Godot property attributes on the generated dto, never on stored values.
    let mut dto_fields = Vec::new();
    let mut field_names = Vec::new();
    let mut default_values = Vec::new();
    let mut exposed_values = Vec::new();
    let mut captured_values = Vec::new();
    let mut cloned_values = Vec::new();
    for field in &mut fields.named {
        let field_name = field.ident.clone().unwrap();
        field_names.push(field_name.clone());
        cloned_values.push(copy_field(&field.ty, quote!(self.#field_name)));
        let nested = field
            .attrs
            .iter()
            .any(|attribute| attribute.path().is_ident("nested"));
        let mut dto_field = field.clone();
        let mut default = quote!(Default::default());
        for attribute in &field.attrs {
            if attribute.path().is_ident("init") {
                let parsed = attribute.parse_args::<MetaNameValue>()?;
                if !parsed.path.is_ident("val") {
                    return Err(Error::new_spanned(
                        attribute,
                        "use #[init(val = ...)] for a field default",
                    ));
                }
                let expression = parsed.value;
                default = quote!(#expression);
            }
        }
        if nested {
            dto_field.ty = parse2(dto_type(&field.ty))?;
            dto_field.attrs.retain(|attribute| {
                !["nested", "init"]
                    .iter()
                    .any(|name| attribute.path().is_ident(name))
            });
            let ty = &field.ty;
            let initial = expose(ty, quote!({ let value: #ty = #default; value }));
            dto_field.attrs.push(parse_quote!(#[init(val = #initial)]));
            exposed_values.push(expose(ty, quote!(self.#field_name)));
            captured_values.push(capture_snapshot(ty, quote!(self.#field_name)));
        } else {
            exposed_values.push(quote!(self.#field_name));
            captured_values.push(copy_field(&field.ty, quote!(self.#field_name)));
        }
        dto_fields.push(dto_field);
        default_values.push(default);
        field.attrs.retain(|attribute| {
            !["var", "export", "init", "nested"]
                .iter()
                .any(|name| attribute.path().is_ident(name))
        });
    }
    let gd = format_ident!("{name}Gd");
    let ref_counted = format_ident!("{name}RefCounted");
    let godot_class = format_ident!("{name}GodotClass");
    let value_trait = format_ident!("{name}TransferSnapshot");
    let dto_trait = format_ident!("{name}TransferDto");
    let convert = format_ident!("{name}GodotConvert");
    let from_godot = format_ident!("{name}FromGodot");
    let by_value = format_ident!("{name}ByValue");
    let error = format_ident!("{name}ConvertError");
    let to_godot = format_ident!("{name}ToGodot");
    // The generated dto is a boundary copy; modifying it cannot mutate a queued value.
    Ok(quote! {
        use godot::obj::Gd as #gd;
        use godot::classes::RefCounted as #ref_counted;
        use godot::meta::{GodotConvert as #convert, FromGodot as #from_godot, ByValue as #by_value};
        use godot::meta::error::ConvertError as #error;
        use godot::meta::ToGodot as #to_godot;
        use godot::prelude::GodotClass as #godot_class;
        use bevy_godot4::prelude::{TransferSnapshot as #value_trait, TransferDto as #dto_trait};
        #value
        impl Clone for #name { fn clone(&self) -> Self { Self { #(#field_names: #cloned_values,)* } } }
        impl Default for #name {
            fn default() -> Self { Self { #(#field_names: #default_values,)* } }
        }
        #[derive(#godot_class, Debug)]
        #[class(init, base=RefCounted)]
        #visibility struct #dto {
            #(#dto_fields,)*
            #[base]
            base: godot::obj::Base<#ref_counted>,
        }
        impl #value_trait for #name {
            type Dto = #dto;
            fn into_dto(self) -> #gd<#dto> {
                #gd::from_init_fn(|base| #dto { base, #(#field_names: #exposed_values,)* })
            }
        }
        impl #dto_trait for #dto {
            type Snapshot = #name;
            fn capture_snapshot(&self) -> #name {
                #name { #(#field_names: #captured_values,)* }
            }
        }
        impl #convert for #name { type Via = #gd<#dto>; }
        impl #to_godot for #name {
            type Pass = #by_value;
            fn to_godot(&self) -> Self::Via { self.clone().into_dto() }
        }
        impl #from_godot for #name {
            fn try_from_godot(value: Self::Via) -> Result<Self, #error> {
                Ok(value.bind().capture_snapshot())
            }
        }
        // Rust callers use the same conversion spelling in either direction.
        impl From<#name> for #gd<#dto> {
            fn from(value: #name) -> Self { value.into_dto() }
        }
        impl From<&#name> for #gd<#dto> {
            fn from(value: &#name) -> Self { value.to_godot_owned() }
        }
        impl From<#gd<#dto>> for #name {
            fn from(value: #gd<#dto>) -> Self { value.bind().capture_snapshot() }
        }
        impl From<&#gd<#dto>> for #name {
            fn from(value: &#gd<#dto>) -> Self { value.bind().capture_snapshot() }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::{Options, generate};
    use syn::{File, parse_quote, parse_str, parse2};

    #[test]
    fn nested_optional_and_list_values_generate_typed_views() {
        let output = generate(
            parse_str::<Options>("ExampleDto").unwrap(),
            parse_quote! {
                struct ExampleSnapshot {
                    #[var] #[nested] child: Option<ChildSnapshot>,
                    #[var] #[nested] children: Vec<ChildSnapshot>,
                }
            },
        )
        .unwrap();
        parse2::<File>(output.clone()).unwrap();
        let text = output.to_string();
        assert!(text.contains("TransferDto"));
        assert!(text.contains("iter_shared"));
        assert!(text.contains("into_dto"));
    }

    #[test]
    fn invalid_declarations_report_errors_instead_of_panicking() {
        assert!(parse_str::<Options>("ExampleDto, Node").is_err());
        assert!(parse_str::<Options>("ExampleDto, Resource").is_err());
        assert!(parse_str::<Options>("ExampleDto, RefCounted").is_err());
        assert!(
            generate(
                parse_str("ExampleDto").unwrap(),
                parse_quote!(
                    struct Example(i32);
                )
            )
            .is_err()
        );
        assert!(
            generate(
                parse_str("ExampleDto").unwrap(),
                parse_quote!(
                    struct Example<T> {
                        field: T,
                    }
                )
            )
            .is_err()
        );
        assert!(
            generate(
                parse_str("ExampleDto").unwrap(),
                parse_quote!(
                    struct Example {
                        #[init(wrong = 1)]
                        field: i32,
                    }
                )
            )
            .is_err()
        );
    }
}
