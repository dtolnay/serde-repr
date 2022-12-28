use proc_macro2::Span;
use syn::{Variant, Result, Attribute, Meta, NestedMeta};
use syn::punctuated::Punctuated;

pub(crate) fn is_serde_other_attribute(input: &Attribute) -> bool {
    if let Ok(meta) = input.parse_meta() {
        if let Meta::List(list) = meta {
            if list.path.is_ident("serde") {
                return list.nested.iter()
                    .any(|nested| {
                        match nested {
                            NestedMeta::Meta(Meta::Path(path)) => {
                                path.is_ident("other")
                            }
                            _ => false,
                        }
                    })
            }
        }
    }

    false
}

pub fn parse_other_variant<T>(variants: &Punctuated<Variant, T>) -> Result<Option<&Variant>> {
    let variants = variants.into_iter()
        .filter(|variant| {
            variant.attrs.iter()
                .any(is_serde_other_attribute)
        })
        .collect::<Vec<_>>();

    if variants.len() > 1 {
        Err(syn::Error::new(Span::call_site(), "Only one variant can be marked with #[serde(other)]"))
    } else {
        Ok(variants.first().map(|v| *v))
    }
}
