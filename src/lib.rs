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

#![allow(clippy::single_match_else)]

extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{BinOp, Data, DeriveInput, Error, Expr, ExprBinary, ExprLit, Fields, Lit, LitInt, parse_macro_input, parse_quote, Result};
use syn::token::Add;

use crate::parse::repr::parse_repr;
use crate::parse::serde_style::Tagged;
use crate::transform::generic::{add_lifetime_bounds, add_trait_bounds};
use crate::transform::variant::struct_from_variant;
use crate::util::find_unused_lifetime;

pub(crate) mod parse;
pub(crate) mod transform;
pub(crate) mod util;

fn derive_serialize_impl(input: DeriveInput) -> Result<TokenStream> {
    let data = if let Data::Enum(e) = &input.data {
        e
    } else {
        return Err(Error::new_spanned(
            input,
            "serde_repr can only be used with enums",
        ));
    };

    let input_ident = &input.ident;
    let serde_style = Tagged::from_attrs(&input.attrs)?;
    let repr_type = parse_repr(&input.attrs)?;
    let mut index = 0;
    let mut last = None;
    let mut variants = Vec::new();
    for (variant_index, variant) in data.variants.iter().enumerate() {
        let variant_index = variant_index as u32;
        let variant_ident = &variant.ident;
        if let Some((_, e)) = &variant.discriminant {
            last = Some(e.clone());
            index = 0;
        }
        let discriminant = if let Some(last) = &last {
            Expr::Binary(ExprBinary {
                attrs: vec![],
                left: Box::new(last.clone()),
                op: BinOp::Add(Add::default()),
                right: Box::new(Expr::Lit(ExprLit {
                    attrs: vec![],
                    lit: Lit::Int(LitInt::new(&index.to_string(), Span::call_site())),
                })),
            })
        } else {
            Expr::Lit(ExprLit {
                attrs: vec![],
                lit: Lit::Int(LitInt::new(&index.to_string(), Span::call_site())),
            })
        };

        variants.push(match &variant.fields {
            Fields::Named(fields) => {
                let field_names_destruct = fields.named.iter().map(|field| field.ident.as_ref().unwrap()).collect::<Vec<_>>();
                let field_count = fields.named.len();
                match &serde_style {
                    Tagged::Externally => {
                        quote! {
                            #input_ident::#variant_ident { #(#field_names_destruct, )* } => {
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                let mut __serde_state = serde::Serializer::serialize_struct_variant(
                                    serializer,
                                    stringify!(#input_ident),
                                    #variant_index,
                                    &const_format::formatcp!("{}", __DISCRIMINANT),
                                    #field_count,
                                )?;
                                #(serde::ser::SerializeStructVariant::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#field_names_destruct),
                                    &#field_names_destruct,
                                )?;)*
                                serde::ser::SerializeStructVariant::end(__serde_state)
                            }
                        }
                    }
                    Tagged::Internally { tag } => {
                        let field_count = field_count + 1;
                        quote! {
                            #input_ident::#variant_ident { #(#field_names_destruct, )* } => {
                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    #field_count,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &((#discriminant) as #repr_type),
                                )?;
                                #(serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#field_names_destruct),
                                    &#field_names_destruct,
                                )?;)*
                                serde::ser::SerializeStruct::end(__serde_state)
                            }
                        }
                    }
                    Tagged::Adjacently { tag, content } => {
                        let lifetime = find_unused_lifetime(&input.generics);
                        let adjacently_tagged = struct_from_variant(
                            &input,
                            &variant,
                            Some(&lifetime),
                            vec![
                                syn::parse_quote! { #[derive(serde::Serialize)] },
                            ],
                        )?;
                        quote! {
                            #input_ident::#variant_ident { #(#field_names_destruct, )* } => {
                                #adjacently_tagged

                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    2,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &((#discriminant) as #repr_type),
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#content),
                                    &__AdjacentlyTagged {
                                        #(#field_names_destruct, )*
                                    },
                                )?;
                                serde::ser::SerializeStruct::end(__serde_state)
                            }
                        }
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let field_names_destruct = (0..fields.unnamed.len())
                    .map(|i| Ident::new(&format!("__field{}", i), Span::call_site()))
                    .collect::<Vec<_>>();
                let field_count = fields.unnamed.len();
                match &serde_style {
                    Tagged::Externally => {
                        quote! {
                            #input_ident::#variant_ident (#(#field_names_destruct, )*) => {
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                let mut __serde_state = serde::Serializer::serialize_tuple_variant(
                                    serializer,
                                    stringify!(#input_ident),
                                    #variant_index,
                                    &const_format::formatcp!("{}", __DISCRIMINANT),
                                    #field_count,
                                )?;
                                #(serde::ser::SerializeTupleVariant::serialize_field(
                                    &mut __serde_state,
                                    &#field_names_destruct,
                                )?;)*
                                serde::ser::SerializeTupleVariant::end(__serde_state)
                            }
                        }
                    }
                    Tagged::Internally { .. } => {
                        Err(Error::new(
                            Span::call_site(),
                            "Internally tagged enums with unnamed fields are not supported",
                        ))?
                    }
                    Tagged::Adjacently { tag, content } => {
                        let lifetime = find_unused_lifetime(&input.generics);
                        let adjacently_tagged = struct_from_variant(
                            &input,
                            &variant,
                            Some(&lifetime),
                            vec![
                                syn::parse_quote! { #[derive(serde::Serialize)] },
                            ],
                        )?;
                        quote! {
                            #input_ident::#variant_ident ( #(#field_names_destruct, )* ) => {
                                #adjacently_tagged

                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    2,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &((#discriminant) as #repr_type),
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#content),
                                    &__AdjacentlyTagged (
                                        #(#field_names_destruct, )*
                                    ),
                                )?;
                                serde::ser::SerializeStruct::end(__serde_state)
                            }
                        }
                    }
                }
            }
            Fields::Unit => {
                match &serde_style {
                    Tagged::Externally => {
                        quote! {
                            #input_ident::#variant_ident => {
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                serde::Serialize::serialize(__DISCRIMINANT, serializer)
                            }
                        }
                    }
                    Tagged::Internally { tag } => {
                        quote! {
                            #input_ident::#variant_ident => {
                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    1,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &((#discriminant) as #repr_type),
                                )?;
                                serde::ser::SerializeStruct::end(__serde_state)
                            }
                        }
                    }
                    Tagged::Adjacently { tag, content } => {
                        quote! {
                            #input_ident::#variant_ident => {
                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    2,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &((#discriminant) as #repr_type),
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#content),
                                    &((#discriminant) as #repr_type),
                                )?;
                                serde::ser::SerializeStruct::end(__serde_state)
                            }
                        }
                    }
                }
            }
        });
        index += 1;
    }

    let generics = add_trait_bounds(input.generics, &parse_quote! { serde::Serialize });
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    Ok(TokenStream::from(quote! {
        impl #impl_generics serde::Serialize for #input_ident #ty_generics #where_clause {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                match self {
                    #(#variants,)*
                }
            }
        }
    }))
}

#[proc_macro_derive(Serialize_repr, attributes(serde))]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    match derive_serialize_impl(derive_input) {
        Ok(tokens) => tokens,
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}

fn derive_deserialize_impl(input: DeriveInput) -> Result<TokenStream> {
    let data = if let Data::Enum(e) = &input.data {
        e
    } else {
        return Err(Error::new_spanned(
            input,
            "serde_repr can only be used with enums",
        ));
    };

    let input_ident = &input.ident;
    let serde_style = Tagged::from_attrs(&input.attrs)?;
    let repr_type = parse_repr(&input.attrs)?;

    let generics = add_trait_bounds(input.generics, &parse_quote! { serde::Deserialize<'de> });
    let generics = add_lifetime_bounds(generics, &parse_quote! { 'de });
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    Ok(TokenStream::from(quote! {
        impl #impl_generics serde::Deserialize<'de> for #input_ident #ty_generics #where_clause {
            #[allow(clippy::use_self)]
            fn deserialize<D>(deserializer: D) -> core::result::Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
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
    }))
}

#[proc_macro_derive(Deserialize_repr, attributes(serde))]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    match derive_deserialize_impl(derive_input) {
        Ok(tokens) => tokens,
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}
