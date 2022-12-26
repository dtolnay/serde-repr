use syn::{Attribute, Lit, Meta, MetaNameValue, NestedMeta, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Tagged {
    Externally,
    /// Create a field in a struct with the specified name and the repr value as value.
    Internally { tag: String },
    /// Create a field in a struct with the specified name and the repr value as value.
    /// And a field with the content of the enum.
    Adjacently { tag: String, content: String },
}

impl Tagged {
    fn union((ll, lr): (Option<String>, Option<String>), (rl, rr): (Option<String>, Option<String>)) -> (Option<String>, Option<String>) {
        (
            if let Some(rl) = rl {
                Some(rl)
            } else {
                ll
            },
            if let Some(rr) = rr {
                Some(rr)
            } else {
                lr
            },
        )
    }

    fn options_to_tagged(options: (Option<String>, Option<String>)) -> Self {
        match options {
            (Some(tag), Some(content)) => Tagged::Adjacently { tag, content },
            (Some(tag), None) => Tagged::Internally { tag },
            (None, _) => Tagged::Externally,
        }
    }

    fn parse_name_value(name_value: &MetaNameValue) -> (Option<String>, Option<String>) {
        if let Lit::Str(lit_str) = &name_value.lit {
            // #[serde(tag = "...")]
            if name_value.path.is_ident("tag") {
                return (Some(lit_str.value()), None);
            }
            // #[serde(content = "...")]
            if name_value.path.is_ident("content") {
                return (None, Some(lit_str.value()));
            }
        }

        (None, None)
    }

    fn parse_attribute(attribute: &Attribute) -> Result<(Option<String>, Option<String>)> {
        // #[serde(...)]
        if attribute.path.is_ident("serde") {
            let meta = attribute.parse_meta()?;
            if let Meta::List(list) = attribute.parse_meta()? {
                // #[serde(tag = "...", content = "...")]
                return Ok(list.nested
                    .into_iter()
                    .filter_map(|nested_meta| {
                        if let NestedMeta::Meta(Meta::NameValue(name_value)) = nested_meta {
                            Some(Tagged::parse_name_value(&name_value))
                        } else {
                            None
                        }
                    })
                    .fold((None, None), Tagged::union));
            }
            if let Meta::NameValue(name_value) = meta {
                // #[serde(tag = "...")]
                // #[serde(content = "...")]
                return Ok(Tagged::parse_name_value(&name_value))
            }
        }

        Ok((None, None))
    }

    pub fn from_attrs(from_attrs: &Vec<Attribute>) -> Result<Self> {
        let options = from_attrs.iter()
            .map(Tagged::parse_attribute)
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .fold((None, None), Tagged::union);

        Ok(Tagged::options_to_tagged(options))
    }
}
