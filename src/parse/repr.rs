use proc_macro2::{Ident, Span};
use syn::{Attribute, Error, Lit, Meta, NestedMeta, Result};

pub(crate) fn parse_repr_from_attribute(input: &Attribute) -> Result<Ident> {
    let meta = input.parse_meta()?;
    match meta {
        Meta::List(list) => {
            if list.path.is_ident("repr") {
                if list.nested.len() != 1 {
                    return Err(Error::new(Span::call_site(), "expected exactly one argument"));
                }
                match list.nested.first().unwrap() {
                    NestedMeta::Meta(Meta::Path(path)) => {
                        if path.is_ident("C") {
                            Ok(syn::parse_quote! { u8 })
                        } else {
                            path.segments.first().map(|s| s.ident.clone()).ok_or_else(|| {
                                Error::new(Span::call_site(), "expected a single identifier")
                            })
                        }
                    }
                    NestedMeta::Lit(Lit::Str(lit)) => {
                        if lit.value() == "C" {
                            Ok(syn::parse_quote! { u8 })
                        } else {
                            Err(Error::new(Span::call_site(), "missing #[repr(C)] attribute or #[repr(...)] attribute with a single identifier"))
                        }
                    }
                    _ => Err(Error::new(Span::call_site(), "missing #[repr(C)] attribute or #[repr(...)] attribute with a single identifier")),
                }
            } else {
                Err(Error::new(Span::call_site(), "missing #[repr(...)] attribute"))
            }
        }
        _ => Err(Error::new(Span::call_site(), "missing #[repr(...)] attribute")),
    }
}

pub(crate) fn parse_repr(input: &Vec<Attribute>) -> Result<Ident> {
    input.into_iter()
        .map(parse_repr_from_attribute)
        .fold(
            Err(Error::new(Span::call_site(), "missing #[repr(...)] attribute")),
            |acc, x| {
                match (acc, x) {
                    (Ok(a), _) => Ok(a),
                    (Err(_), Ok(b)) => Ok(b),
                    (Err(_), Err(e)) => Err(e),
                }
        })
}
