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

mod parse;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{Fields, parse_macro_input, DeriveInput, GenericParam, parse_quote, Generics, Type, Field, Meta, Lit, Result, Ident, LitStr, Data, DataStruct, Visibility, FieldsNamed, NestedMeta, TypeParam, LifetimeDef, Error, FieldsUnnamed, Lifetime};
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::token::{Brace, Paren};

use crate::parse::Input;

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(serde::Serialize));
        }
    }
    generics
}

fn add_lifetime_bounds(mut generics: Generics) -> (Generics, Lifetime) {
    const CHARS: &str = "abcdefghijklmnopqrstuvwxyz";
    let mut i = 1;
    loop {
        // Generate string from number
        let mut s = String::new();
        let mut n = i;
        s.push('\'');
        while n > 0 {
            s.push(CHARS.chars().nth(n % CHARS.len()).unwrap());
            n /= CHARS.len();
        }
        let lifetime = Lifetime::new(&s, Span::call_site());

        let mut found = false;
        for param in &mut generics.params {
            if let GenericParam::Lifetime(ref mut lifetime_param) = *param {
                if lifetime_param.lifetime == lifetime {
                    found = true;
                }
            }
        }

        if !found {
            generics.params.push(GenericParam::Lifetime(LifetimeDef::new(lifetime.clone())));
            return (generics, lifetime);
        }
    }
}

fn parse_repr(input: &DeriveInput) -> Result<Ident> {
    let call_site = Span::call_site();
    let repr = input.attrs.iter().find(|attr| attr.path.is_ident("repr"))
        .ok_or_else(|| Error::new(call_site, "missing #[repr(...)] attribute"))?
        .parse_meta()?;

    if let Meta::List(list) = repr {
        let nested = list.nested.first().ok_or_else(|| Error::new(call_site, "missing #[repr(...)] attribute"))?;
        if let NestedMeta::Meta(meta) = nested {
            if meta.path().segments.len() != 1 {
                Err(Error::new_spanned(meta, "invalid #[repr(...)] attribute"))
            } else {
                Ok(
                    meta.path().segments.first()
                        .ok_or_else(|| Error::new_spanned(meta, "invalid #[repr(...)] attribute"))?
                        .ident
                        .clone()
                )
            }
        } else {
            Err(Error::new_spanned(nested, "expected literal"))
        }
    } else {
        Err(Error::new_spanned(repr, "repr attribute illformed"))?
    }
}

fn parse_serde_tag(input: &DeriveInput) -> Result<LitStr> {
    let call_site = Span::call_site();
    let tag = input.attrs.iter().find(|attr| attr.path.is_ident("serde"))
        .ok_or_else(|| Error::new(call_site, "missing #[serde(tag = \"...\")]"))?
        .parse_meta()?;

    if let Meta::List(list) = tag {
        let tag = list.nested.iter().find(|meta| {
            if let NestedMeta::Meta(meta) = meta {
                if meta.path().is_ident("tag") {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }).ok_or_else(|| Error::new_spanned(&list, "missing tag = \"...\""))?;

        if let NestedMeta::Meta(name_value) = tag {
            if let Meta::NameValue(name_value) = name_value {
                if let Lit::Str(lit_str) = &name_value.lit {
                    Ok(lit_str.clone())
                } else {
                    Err(Error::new_spanned(&name_value.lit, "tag value must be a string"))?
                }
            } else {
                Err(Error::new_spanned(name_value, "tag value must be a string"))?
            }
        } else {
            Err(Error::new_spanned(tag, "tag value must be a string"))?
        }
    } else {
        Err(Error::new_spanned(tag, "repr attribute illformed"))?
    }
}

fn type_contains_generic(type_param: &TypeParam, t: &Type) -> bool {
    match t {
        Type::Path(type_path) => {
            type_path.path.segments.iter().any(|segment| {
                segment.ident == type_param.ident
            })
        },
        Type::Reference(type_reference) => {
            type_contains_generic(type_param, &type_reference.elem)
        },
        Type::Tuple(type_tuple) => {
            type_tuple.elems.iter().any(|t| {
                type_contains_generic(type_param, t)
            })
        },
        Type::Paren(type_paren) => {
            type_contains_generic(type_param, &type_paren.elem)
        },
        Type::Group(type_group) => {
            type_contains_generic(type_param, &type_group.elem)
        },
        Type::Array(type_array) => {
            type_contains_generic(type_param, &type_array.elem)
        },
        Type::Slice(type_slice) => {
            type_contains_generic(type_param, &type_slice.elem)
        },
        Type::Ptr(type_ptr) => {
            type_contains_generic(type_param, &type_ptr.elem)
        },
        Type::BareFn(type_bare_fn) => {
            type_bare_fn.inputs.iter().any(|t| {
                let t = &t.ty;
                type_contains_generic(type_param, t)
            }) || match &type_bare_fn.output {
                syn::ReturnType::Default => false,
                syn::ReturnType::Type(_, t) => type_contains_generic(type_param, t),
            }
        },
        Type::TraitObject(type_trait_object) => {
            type_trait_object.bounds.iter().any(|t| {
                match t {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        trait_bound.path.segments.iter().any(|segment| {
                            segment.ident == type_param.ident
                        })
                    },
                    _ => false,
                }
            })
        },
        Type::ImplTrait(type_impl_trait) => {
            type_impl_trait.bounds.iter().any(|t| {
                match t {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        trait_bound.path.segments.iter().any(|segment| {
                            segment.ident == type_param.ident
                        })
                    },
                    _ => false,
                }
            })
        },
        Type::Macro(type_macro) => {
            type_macro.mac.path.segments.iter().any(|segment| {
                segment.ident == type_param.ident
            })
        },
        _ => {
            false
        },
    }
}

fn type_contains_generic_lifetime(type_param: &LifetimeDef, t: &Type) -> bool {
    match t {
        Type::Path(type_path) => {
            type_path.path.segments.iter().any(|segment| {
                segment.ident == type_param.lifetime.ident
            })
        },
        Type::Reference(type_reference) => {
            if let Some(lifetime) = &type_reference.lifetime {
                lifetime.ident == type_param.lifetime.ident
            } else {
                false
            }
        },
        Type::Tuple(type_tuple) => {
            type_tuple.elems.iter().any(|t| {
                type_contains_generic_lifetime(type_param, t)
            })
        },
        Type::Paren(type_paren) => {
            type_contains_generic_lifetime(type_param, &type_paren.elem)
        },
        Type::Group(type_group) => {
            type_contains_generic_lifetime(type_param, &type_group.elem)
        },
        Type::Array(type_array) => {
            type_contains_generic_lifetime(type_param, &type_array.elem)
        },
        Type::Slice(type_slice) => {
            type_contains_generic_lifetime(type_param, &type_slice.elem)
        },
        Type::Ptr(type_ptr) => {
            type_contains_generic_lifetime(type_param, &type_ptr.elem)
        },
        Type::BareFn(type_bare_fn) => {
            type_bare_fn.inputs.iter().any(|t| {
                let t = &t.ty;
                type_contains_generic_lifetime(type_param, t)
            }) || match &type_bare_fn.output {
                syn::ReturnType::Default => false,
                syn::ReturnType::Type(_, t) => type_contains_generic_lifetime(type_param, t),
            } || if let Some(lifetimes) = &type_bare_fn.lifetimes {
                lifetimes.lifetimes.iter().any(|lifetime| {
                    lifetime.lifetime.ident == type_param.lifetime.ident
                })
            } else {
                false
            }
        },
        Type::TraitObject(type_trait_object) => {
            type_trait_object.bounds.iter().any(|t| {
                match t {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        trait_bound.path.segments.iter().any(|segment| {
                            segment.ident == type_param.lifetime.ident
                        })
                    },
                    _ => false,
                }
            })
        },
        Type::ImplTrait(type_impl_trait) => {
            type_impl_trait.bounds.iter().any(|t| {
                match t {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        trait_bound.path.segments.iter().any(|segment| {
                            segment.ident == type_param.lifetime.ident
                        })
                    },
                    _ => false,
                }
            })
        },
        Type::Macro(type_macro) => {
            type_macro.mac.path.segments.iter().any(|segment| {
                segment.ident == type_param.lifetime.ident
            })
        },
        _ => false
    }
}

fn filter_generics_by_fields(generics: Generics, fields: Vec<&Field>) -> Result<Generics> {
    let mut filtered = Generics::default();
    for param in generics.params {
        match param {
            GenericParam::Type(type_param) => {
                for field in &fields {
                    let ty: &Type = &field.ty;
                    if type_contains_generic(&type_param, ty) {
                        filtered.params.push(GenericParam::Type(type_param));
                        break;
                    }
                }
            }
            GenericParam::Lifetime(lifetime) => {
                for field in &fields {
                    let ty: &Type = &field.ty;
                    if type_contains_generic_lifetime(&lifetime, ty) {
                        filtered.params.push(GenericParam::Lifetime(lifetime));
                        break;
                    }
                }
            }
            GenericParam::Const(param) => {
                filtered.params.push(GenericParam::Const(param));
            }
        }
    }
    Ok(filtered)
}

#[proc_macro_derive(Serialize_repr, attributes(serde))]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let call_site = Span::call_site();

    // Used in the quasi-quotation below as `#name`.
    let name = &input.ident;

    let data = if let Data::Enum(data) = &input.data {
        if data.variants.len() > 0 {
            data
        } else {
            return TokenStream::from(Error::new(
                call_site,
                "there must be at least one variant",
            ).into_compile_error());
        }
    } else {
        return TokenStream::from(Error::new(
            call_site,
            "input must be an enum",
        ).into_compile_error());
    };

    let repr = match parse_repr(&input) {
        Ok(repr) => repr,
        Err(e) => return TokenStream::from(e.into_compile_error()),
    };
    let serde_tag = match parse_serde_tag(&input) {
        Ok(serde_tag) => Some(serde_tag),
        Err(_) => None,
    }.unwrap();

    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut all_unit_variants = true;
    for variant in &data.variants {
        if let Fields::Unit = variant.fields {
            continue;
        } else {
            all_unit_variants = false;
            break;
        }
    }

    // Generate an expression to sum up the heap size of each field.
    let mut arms = Vec::new();
    for variant in &data.variants {
        let ident = &variant.ident;

        arms.push(match variant.fields {
            Fields::Named(ref fields) => {
                let generics: Generics = filter_generics_by_fields(
                    generics.clone(),
                    fields.named.iter().collect()
                ).unwrap();
                let generics: Generics = add_trait_bounds(generics);
                let (generics, lifetime) = add_lifetime_bounds(generics);
                let named_struct_ident = Ident::new(&format!("{}__", ident), ident.span());
                let named_struct = DeriveInput {
                    ident: named_struct_ident.clone(),
                    vis: Visibility::Inherited,
                    attrs: vec![parse_quote!(#[derive(serde::Serialize)])],
                    generics,
                    data: Data::Struct(DataStruct {
                        fields: Fields::Named(FieldsNamed {
                            brace_token: Brace::default(),
                            named: (|| {
                                let mut named_fields = Punctuated::new();

                                named_fields.push(Field {
                                    attrs: vec![
                                        parse_quote!(#[serde(rename = #serde_tag)]),
                                    ],
                                    vis: Visibility::Inherited,
                                    ident: Some(Ident::new("type__", ident.span())),
                                    colon_token: None,
                                    ty: parse_quote!(#repr),
                                });
                                for field in &fields.named {
                                    let mut field = field.clone();
                                    let ty = &field.ty;
                                    field.ty = parse_quote!(&#lifetime #ty);
                                    named_fields.push(field);
                                }

                                named_fields
                            })(),
                        }),
                        semi_token: Some(parse_quote!(;)),
                        struct_token: parse_quote!(struct),
                    }),
                };
                let fields = fields.named.iter().map(|field| {
                    field.ident.as_ref().unwrap()
                }).collect::<Vec<_>>();

                quote! {
                    #name::#ident { #(#fields)* } => {
                        #named_struct

                        let value = #named_struct_ident {
                            type__: unsafe { *<*const _>::from(self).cast::<#repr>() },
                            #(#fields,)*
                        };

                        value.serialize(serializer)
                    }
                }
            }
            Fields::Unnamed(ref fields) => {
                let generics: Generics = filter_generics_by_fields(
                    generics.clone(),
                    fields.unnamed.iter().collect()
                ).unwrap();
                let generics: Generics = add_trait_bounds(generics);
                let (generics, lifetime) = add_lifetime_bounds(generics);
                let unnamed_struct_ident = Ident::new(&format!("{}__", ident), ident.span());
                let unnamed_struct = DeriveInput {
                    ident: unnamed_struct_ident.clone(),
                    vis: Visibility::Inherited,
                    attrs: vec![parse_quote!(#[derive(serde::Serialize)])],
                    generics,
                    data: Data::Struct(DataStruct {
                        fields: Fields::Unnamed(FieldsUnnamed {
                            paren_token: Paren::default(),
                            unnamed: (|| {
                                let mut unnamed_fields = Punctuated::new();

                                unnamed_fields.push(Field {
                                    attrs: vec![],
                                    vis: Visibility::Inherited,
                                    ident: None,
                                    colon_token: None,
                                    ty: parse_quote!(#repr),
                                });
                                for field in &fields.unnamed {
                                    let mut field = field.clone();
                                    let ty = &field.ty;
                                    field.ty = parse_quote!(&#lifetime #ty);
                                    unnamed_fields.push(field);
                                }

                                unnamed_fields
                            })(),
                        }),
                        semi_token: Some(parse_quote!(;)),
                        struct_token: parse_quote!(struct),
                    }),
                };
                let fields = (0..fields.unnamed.len()).into_iter().map(|i| {
                    Ident::new(&format!("f{}", i), proc_macro2::Span::call_site())
                }).collect::<Vec<_>>();

                quote! {
                    #name::#ident (#(#fields)*) => {
                        #unnamed_struct

                        let value = #unnamed_struct_ident(
                            unsafe { *<*const _>::from(self).cast::<#repr>() },
                            #(#fields,)*
                        );

                        value.serialize(serializer)
                    }
                }
            }
            Fields::Unit => {
                if all_unit_variants {
                    quote! {
                        #name::#ident => {
                            let value = #name::#ident as #repr;

                            value.serialize(serializer)
                        }
                    }
                } else {
                    quote! {
                        #name::#ident => {
                            let value = unsafe { *<*const _>::from(self).cast::<#repr>() };

                            value.serialize(serializer)
                        }
                    }
                }
            },
        });
    }

    TokenStream::from(quote! {
        impl #impl_generics serde::Serialize for #name #ty_generics #where_clause {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                match self {
                    #(#arms,)*
                }
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

    let declare_discriminants = input.variants.iter().map(|variant| {
        let variant = &variant.ident;
        quote! {
            const #variant: #repr = #ident::#variant as #repr;
        }
    });

    let match_discriminants = input.variants.iter().map(|variant| {
        let variant = &variant.ident;
        quote! {
            discriminant::#variant => core::result::Result::Ok(#ident::#variant),
        }
    });

    let error_format = match input.variants.len() {
        1 => "invalid value: {}, expected {}".to_owned(),
        2 => "invalid value: {}, expected {} or {}".to_owned(),
        n => "invalid value: {}, expected one of: {}".to_owned() + &", {}".repeat(n - 1),
    };

    let other_arm = match input.default_variant {
        Some(variant) => {
            let variant = &variant.ident;
            quote! {
                core::result::Result::Ok(#ident::#variant)
            }
        }
        None => quote! {
            core::result::Result::Err(serde::de::Error::custom(
                format_args!(#error_format, other #(, discriminant::#variants)*)
            ))
        },
    };

    TokenStream::from(quote! {
        impl<'de> serde::Deserialize<'de> for #ident {
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
    })
}
