use std::collections::HashSet;
use syn::{
    Error, Expr, Ident, LitStr, Path, Result, Token, Type, braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

pub(super) struct Field {
    pub name: Ident,
    pub ty: Type,
}
impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(Self {
            name,
            ty: input.parse()?,
        })
    }
}

pub(super) enum Source {
    Existing {
        config: Path,
        name: Ident,
    },
    Declared {
        prefix: Ident,
        input_list: Vec<Field>,
        mapping: Option<(Vec<Field>, Expr)>,
        label: Option<LitStr>,
    },
}
pub(super) struct Spec {
    pub source: Source,
}

fn fields(input: ParseStream) -> Result<Vec<Field>> {
    let content;
    braced!(content in input);
    let field_list = Punctuated::<Field, Token![,]>::parse_terminated(&content)?;
    if field_list.is_empty() {
        return Err(content.error("declare at least one field"));
    }
    let mut name_set = HashSet::new();
    for field in &field_list {
        if !name_set.insert(field.name.to_string()) {
            return Err(Error::new(field.name.span(), "field names must be unique"));
        }
    }
    Ok(field_list.into_iter().collect())
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        // Read named settings and reject duplicates instead of silently replacing them.
        let mut key_set = HashSet::new();
        let mut config = None;
        let mut name = None;
        let mut prefix = None;
        let mut input_list = None;
        let mut output_list = None;
        let mut mapping = None;
        let mut label = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            if !key_set.insert(key.to_string()) {
                return Err(Error::new(key.span(), "reader setting declared twice"));
            }
            input.parse::<Token![:]>()?;
            match key.to_string().as_str() {
                "config" => config = Some(input.parse()?),
                "name" => name = Some(input.parse()?),
                "prefix" => prefix = Some(input.parse()?),
                "inputs" => input_list = Some(fields(input)?),
                "outputs" => output_list = Some(fields(input)?),
                "value_mapping_fn" => mapping = Some(input.parse()?),
                "label" => label = Some(input.parse()?),
                _ => return Err(Error::new(key.span(), "unknown reader setting")),
            }
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }

        // A declaration either defines the reader or exposes an existing config.
        let source = if let Some(prefix) = prefix {
            if config.is_some() || name.is_some() {
                return Err(input.error("prefix cannot be combined with config or name"));
            }
            let input_list = input_list.ok_or_else(|| input.error("prefix requires inputs"))?;
            let mapping = match (output_list, mapping) {
                (None, None) => None,
                (Some(fields), Some(mapping)) => Some((fields, mapping)),
                _ => {
                    return Err(
                        input.error("outputs and value_mapping_fn must be supplied together")
                    );
                }
            };
            Source::Declared {
                prefix,
                input_list,
                mapping,
                label,
            }
        } else {
            if input_list.is_some() || output_list.is_some() || mapping.is_some() || label.is_some()
            {
                return Err(
                    input.error("inputs, outputs, value_mapping_fn and label require prefix")
                );
            }
            Source::Existing {
                config: config.ok_or_else(|| input.error("expected prefix or config"))?,
                name: name.ok_or_else(|| input.error("config requires name"))?,
            }
        };
        Ok(Self { source })
    }
}

#[cfg(test)]
mod tests {
    use super::Spec;
    use syn::parse_str;

    #[test]
    fn rejects_ambiguous_or_incomplete_declarations() {
        for source in [
            "prefix: A, config: C, inputs: { x: T }",
            "prefix: A, inputs: { x: T }, outputs: { x: f32 }",
            "prefix: A, inputs: { x: T }, value_mapping_fn: |x| Some(x)",
            "prefix: A, inputs: { x: T, x: U }",
            "prefix: A, inputs: {}",
            "prefix: A, prefix: B, inputs: { x: T }",
            "config: C, name: A, methods: {}",
            r#"config: C, name: A, label: "unexpected""#,
        ] {
            assert!(parse_str::<Spec>(source).is_err(), "accepted {source}");
        }
    }
}
