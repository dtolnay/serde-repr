//! [![github]](https://github.com/dtolnay/serde-repr)&ensp;[![crates-io]](https://crates.io/crates/serde_repr)&ensp;[![docs-rs]](https://docs.rs/serde_repr)
//!
//! [github]: https://img.shields.io/badge/github-8da0cb?style=for-the-badge&labelColor=555555&logo=github
//! [crates-io]: https://img.shields.io/badge/crates.io-fc8d62?style=for-the-badge&labelColor=555555&logo=rust
//! [docs-rs]: https://img.shields.io/badge/docs.rs-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs
//!
//! <br>
//!
//! Derive `Serialize` and `Deserialize` that delegates to the underlying repr
//! of a C-like enum.
//!
//! # Examples
//!
//! ```
//! use serde_repr::{Serialize_repr, Deserialize_repr};
//!
//! #[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug)]
//! #[repr(u8)]
//! enum SmallPrime {
//!     Two = 2,
//!     Three = 3,
//!     Five = 5,
//!     Seven = 7,
//! }
//!
//! fn main() -> serde_json::Result<()> {
//!     let j = serde_json::to_string(&SmallPrime::Seven)?;
//!     assert_eq!(j, "7");
//!
//!     let p: SmallPrime = serde_json::from_str("2")?;
//!     assert_eq!(p, SmallPrime::Two);
//!
//!     Ok(())
//! }
//! ```

#![doc(html_root_url = "https://docs.rs/serde_repr/0.1.20")]
#![allow(clippy::single_match_else)]

extern crate proc_macro;

mod parse;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

use crate::parse::Input;

fn build_numerics_enum(
    ident: &syn::Ident,
    variants: &[parse::Variant],
) -> proc_macro2::TokenStream {
    let fields = variants.iter().map(|variant| {
        let variant_ident = &variant.ident;
        if variant.attrs.is_untagged {
            quote! {}
        } else {
            if let Some(discriminant) = &variant.discriminant {
                let expr = &discriminant.1;
                quote! {
                    #variant_ident = #expr,
                }
            } else {
                quote! {
                    #variant_ident,
                }
            }
        }
    });
    quote!(
        #[allow(non_camel_case_types)]
        enum #ident {
            #( #fields )*
        }
    )
}

#[proc_macro_derive(Serialize_repr)]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    let ident = input.ident;
    let repr = input.repr;

    let has_untagged_variant = input
        .default_variant
        .as_ref()
        .map_or(false, |v| v.attrs.is_untagged);

    let numeric_ident = if has_untagged_variant {
        syn::Ident::new(&format!("NumericOnly{}", ident.to_string()), ident.span())
    } else {
        ident.clone()
    };
    let numerics_only = if has_untagged_variant {
        build_numerics_enum(&numeric_ident, &input.variants)
    } else {
        quote! {}
    };

    let match_variants = input.variants.iter().map(|variant| {
        let variant_ident = &variant.ident;
        if variant.attrs.is_untagged {
            quote! {
                #ident::#variant_ident(inner) => inner as #repr,
            }
        } else {
            quote! {
                #ident::#variant_ident => #numeric_ident::#variant_ident as #repr,
            }
        }
    });
    TokenStream::from(quote! {
        #[allow(deprecated)]
        impl serde::Serialize for #ident {
            #[allow(clippy::use_self)]
            fn serialize<S>(&self, serializer: S) -> ::core::result::Result<S::Ok, S::Error>
            where
                S: serde::Serializer
            {
                #numerics_only

                let value: #repr = match *self {
                    #(#match_variants)*
                };
                serde::Serialize::serialize(&value, serializer)
            }
        }
    })
}

#[proc_macro_derive(Deserialize_repr, attributes(serde))]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    let ident = input.ident;
    let repr = input.repr;
    let variants = input.variants.iter().map(|variant| &variant.ident);

    let has_untagged_variant = input
        .default_variant
        .as_ref()
        .map_or(false, |v| v.attrs.is_untagged);

    let numeric_ident = if has_untagged_variant {
        syn::Ident::new(&format!("NumericOnly{}", ident.to_string()), ident.span())
    } else {
        ident.clone()
    };
    let numerics_only_enum = if has_untagged_variant {
        build_numerics_enum(&numeric_ident, &input.variants)
    } else {
        quote! {}
    };

    let declare_discriminants = input.variants.iter().map(|variant| {
        let variant_ident = &variant.ident;
        if variant.attrs.is_untagged {
            quote! {}
        } else {
            quote! {
                const #variant_ident: #repr = #numeric_ident::#variant_ident as #repr;
            }
        }
    });

    let match_discriminants = input.variants.iter().map(|variant| {
        let variant_ident = &variant.ident;
        if variant.attrs.is_untagged {
            quote! {}
        } else {
            quote! {
                discriminant::#variant_ident => ::core::result::Result::Ok(#ident::#variant_ident),
            }
        }
    });

    let error_format = match input.variants.len() {
        1 => "invalid value: {}, expected {}".to_owned(),
        2 => "invalid value: {}, expected {} or {}".to_owned(),
        n => "invalid value: {}, expected one of: {}".to_owned() + &", {}".repeat(n - 1),
    };

    let other_arm = match input.default_variant {
        Some(variant) => {
            if variant.attrs.is_default {
                let variant = &variant.ident;
                quote! {
                    ::core::result::Result::Ok(#ident::#variant)
                }
            } else if variant.attrs.is_untagged {
                quote! {
                    ::core::result::Result::Ok(#ident::Other(other))
                }
            } else {
                quote! {
                    This Should Never Happen!
                }
            }
        }
        None => quote! {
            ::core::result::Result::Err(serde::de::Error::custom(
                format_args!(#error_format, other #(, discriminant::#variants)*)
            ))
        },
    };

    TokenStream::from(quote! {
        #[allow(deprecated)]
        impl<'de> serde::Deserialize<'de> for #ident {
            #[allow(clippy::use_self)]
            fn deserialize<D>(deserializer: D) -> ::core::result::Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                #numerics_only_enum

                #[allow(non_camel_case_types)]
                struct discriminant;

                #[allow(non_upper_case_globals)]
                impl discriminant {
                    #(#declare_discriminants)*
                }

                match <#repr as serde::Deserialize>::deserialize(deserializer)? {
                    #(#match_discriminants)*
                    other => #other_arm,
                }
            }
        }
    })
}
