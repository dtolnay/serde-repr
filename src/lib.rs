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
use syn::{BinOp, Data, DeriveInput, Error, Expr, ExprBinary, ExprLit, Fields, GenericParam, Lifetime, LifetimeDef, Lit, LitInt, LitStr, parse_macro_input, parse_quote, Result};
use syn::token::Add;

use crate::parse::repr::parse_repr;
use crate::parse::serde_other::parse_other_variant;
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
        return Err(Error::new(
            Span::call_site(),
            "input must be an enum",
        ));
    };

    if data.variants.is_empty() {
        return Err(Error::new(
            Span::call_site(),
            "there must be at least one variant",
        ));
    }

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

        let all_variants_unit = data.variants.iter().all(|v| {
            if let Fields::Unit = v.fields {
                true
            } else {
                false
            }
        });
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
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    #field_count,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &__DISCRIMINANT,
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
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                #adjacently_tagged

                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    2,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &__DISCRIMINANT,
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
                        if field_count == 1 {
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
                                #input_ident::#variant_ident (field_0) => {
                                    const __DISCRIMINANT: #repr_type = #discriminant;
                                    #adjacently_tagged

                                    serde::Serializer::serialize_newtype_variant(
                                        serializer,
                                        stringify!(#input_ident),
                                        #variant_index,
                                        &const_format::formatcp!("{}", __DISCRIMINANT),
                                        &__AdjacentlyTagged (
                                            field_0
                                        ),
                                    )
                                }
                            }
                        } else {
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
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                #adjacently_tagged

                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    2,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &__DISCRIMINANT,
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
                        if all_variants_unit {
                            quote! {
                                #input_ident::#variant_ident => {
                                    const __DISCRIMINANT: #repr_type = #discriminant;
                                    serde::Serialize::serialize(&__DISCRIMINANT, serializer)
                                }
                            }
                        } else {
                            quote! {
                                #input_ident::#variant_ident => {
                                    const __DISCRIMINANT: #repr_type = #discriminant;
                                    serde::Serializer::serialize_unit_variant(
                                        serializer,
                                        stringify!(#input_ident),
                                        #variant_index,
                                        &const_format::formatcp!("{}", __DISCRIMINANT),
                                    )
                                }
                            }
                        }
                    }
                    Tagged::Internally { tag } => {
                        quote! {
                            #input_ident::#variant_ident => {
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    1,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &__DISCRIMINANT,
                                )?;
                                serde::ser::SerializeStruct::end(__serde_state)
                            }
                        }
                    }
                    Tagged::Adjacently { tag, content } => {
                        quote! {
                            #input_ident::#variant_ident => {
                                const __DISCRIMINANT: #repr_type = #discriminant;
                                let mut __serde_state = serde::Serializer::serialize_struct(
                                    serializer,
                                    stringify!(#variant_ident),
                                    2,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#tag),
                                    &__DISCRIMINANT,
                                )?;
                                serde::ser::SerializeStruct::serialize_field(
                                    &mut __serde_state,
                                    stringify!(#content),
                                    &__DISCRIMINANT,
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
        return Err(Error::new(
            Span::call_site(),
            "input must be an enum",
        ));
    };

    if data.variants.is_empty() {
        return Err(Error::new(
            Span::call_site(),
            "there must be at least one variant",
        ));
    }

    let input_ident = &input.ident;
    let serde_style = Tagged::from_attrs(&input.attrs)?;
    let repr_type = parse_repr(&input.attrs)?;
    let other_variant = parse_other_variant(&data.variants)?;

    // Build the variant discriminant consts.
    let mut declare_discriminants = Vec::new();
    let mut index = 0;
    let mut last = None;
    for variant in &data.variants {
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
        let declare_ident = Ident::new(&format!("__{}", variant_ident.to_string().to_uppercase()), Span::call_site());

        declare_discriminants.push(quote! {
            const #declare_ident: #repr_type = #discriminant;
        });
        index += 1;
    }

    let variant_names = data.variants.iter()
        .map(|v| v.ident.to_string())
        .collect::<Vec<_>>();
    let generics = add_trait_bounds(input.generics, &parse_quote! { serde::Deserialize<'de> });
    let generics = add_lifetime_bounds(generics, &parse_quote! { 'de });
    let (_, ty_generics, where_clause) = generics.split_for_impl();
    let mut generics = generics.clone();
    let mut lifetime = LifetimeDef::new(Lifetime::new(
        "'de",
        Span::call_site()
    ));
    for param in &mut generics.params {
        if let GenericParam::Lifetime(lt) = param {
            lifetime.bounds.push(lt.lifetime.clone());
        }
    }
    generics.params.push(GenericParam::Lifetime(lifetime));
    let (impl_generics, _, _) = generics.split_for_impl();
    let all_variants_unit = data.variants.iter().all(|v| {
        if let Fields::Unit = v.fields {
            true
        } else {
            false
        }
    });

    if !all_variants_unit {
        // Build the variant match body.
        let match_branches = data.variants.iter()
            .map(|variant| {
                let variant_ident = &variant.ident;
                let declare_ident = Ident::new(&format!("__{}", variant_ident.to_string().to_uppercase()), Span::call_site());
                let discriminant = if other_variant.as_ref().map_or(false, |v| ((*v) as *const _) == (variant as *const _)) {
                    quote! {
                        _
                    }
                } else {
                    quote! {
                        __Discriminant(__Discriminant::#declare_ident)
                    }
                };
                match &variant.fields {
                    Fields::Named(_) => {
                        match serde_style {
                            Tagged::Externally => {
                                let field_idents = variant.fields.iter()
                                    .map(|f| f.ident.as_ref().unwrap())
                                    .collect::<Vec<_>>();
                                let field_names = variant.fields.iter()
                                    .map(|f| f.ident.as_ref().unwrap().to_string())
                                    .map(|s| LitStr::new(&s, Span::call_site()))
                                    .collect::<Vec<_>>();
                                let field_ids = (0..field_idents.len()).into_iter()
                                    .map(|i| LitInt::new(&i.to_string(), Span::call_site()))
                                    .collect::<Vec<_>>();
                                quote! {
                                (#discriminant, __variant) => {
                                    struct __VariantVisitor #ty_generics {
                                        marker: core::marker::PhantomData<#input_ident #ty_generics >,
                                    }
                                    impl #impl_generics serde::de::Visitor<'de> for __VariantVisitor #ty_generics {
                                        type Value = #input_ident #ty_generics;

                                        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                                            formatter.write_str(stringify!(struct #input_ident::#variant_ident))
                                        }

                                        fn visit_seq<A>(self, mut seq: A) -> core::result::Result<Self::Value, A::Error> where A: serde::de::SeqAccess<'de>, {
                                            #(let #field_idents = seq.next_element()?.ok_or_else(|| serde::de::Error::invalid_length(#field_ids, &self))?;)*
                                            Ok(#input_ident::#variant_ident {
                                                #(#field_idents,)*
                                            })
                                        }
                                        fn visit_map<A>(self, mut map: A) -> core::result::Result<Self::Value, A::Error> where A: serde::de::MapAccess<'de>, {
                                            #(let mut #field_idents = None;)*
                                            while let Some(key) = map.next_key()? {
                                                match key {
                                                    #(#field_names => {
                                                        if #field_idents.is_some() {
                                                            return Err(serde::de::Error::duplicate_field(#field_names));
                                                        }
                                                        #field_idents = Some(map.next_value()?);
                                                    })*
                                                    _ => { let _: serde::de::IgnoredAny = map.next_value()?; }
                                                }
                                            }
                                            #(let #field_idents = #field_idents.ok_or_else(|| serde::de::Error::missing_field(#field_names))?;)*
                                            Ok(#input_ident::#variant_ident {
                                                #(#field_idents,)*
                                            })
                                        }
                                    }

                                    serde::de::VariantAccess::struct_variant(__variant,
                                        &[
                                            #(
                                                #field_names,
                                            )*
                                        ],
                                        __VariantVisitor {
                                            marker: core::marker::PhantomData::<#input_ident #ty_generics>,
                                        }
                                    )
                                }
                            }
                            }
                            Tagged::Internally { .. } => {
                                quote! {
                                (#discriminant, __variant) => {
                                    unimplemented!()
                                }
                            }
                            }
                            Tagged::Adjacently { .. } => {
                                quote! {
                                (#discriminant, __variant) => {
                                    unimplemented!()
                                }
                            }
                            }
                        }
                    }
                    Fields::Unnamed(fields) => {
                        if fields.unnamed.len() == 1 {
                            match serde_style {
                                Tagged::Externally => {
                                    let variant_type = &fields.unnamed.first().unwrap().ty;
                                    quote! {
                                    (#discriminant, __variant) => {
                                        serde::__private::Result::map(
                                            serde::de::VariantAccess::newtype_variant::<#variant_type>(__variant),
                                            #input_ident::#variant_ident
                                        )
                                    }
                                }
                                }
                                Tagged::Internally { .. } => {
                                    quote! {
                                    (#discriminant, __variant) => {
                                        unimplemented!()
                                    }
                                }
                                }
                                Tagged::Adjacently { .. } => {
                                    quote! {
                                    (#discriminant, __variant) => {
                                        unimplemented!()
                                    }
                                }
                                }
                            }
                        } else {
                            match serde_style {
                                Tagged::Externally => {
                                    let field_idents = (0..fields.unnamed.len())
                                        .map(|i| Ident::new(&format!("__field{}", i), Span::call_site()))
                                        .collect::<Vec<_>>();
                                    let field_names = (0..fields.unnamed.len())
                                        .map(|i| LitStr::new(&format!("__field{}", i), Span::call_site()))
                                        .collect::<Vec<_>>();
                                    let field_ids = (0..field_idents.len()).into_iter()
                                        .map(|i| LitInt::new(&i.to_string(), Span::call_site()))
                                        .collect::<Vec<_>>();
                                    quote! {
                                    (#discriminant, __variant) => {
                                        struct __VariantVisitor #ty_generics {
                                            marker: core::marker::PhantomData<#input_ident #ty_generics >,
                                        }
                                        impl #impl_generics serde::de::Visitor<'de> for __VariantVisitor #ty_generics {
                                            type Value = #input_ident #ty_generics;

                                            fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                                                formatter.write_str(stringify!(struct #input_ident::#variant_ident))
                                            }

                                            fn visit_seq<A>(self, mut seq: A) -> core::result::Result<Self::Value, A::Error> where A: serde::de::SeqAccess<'de>, {
                                                #(let #field_idents = seq.next_element()?.ok_or_else(|| serde::de::Error::invalid_length(#field_ids, &self))?;)*
                                                Ok(#input_ident::#variant_ident(
                                                    #(#field_idents,)*
                                                ))
                                            }
                                            fn visit_map<A>(self, mut map: A) -> core::result::Result<Self::Value, A::Error> where A: serde::de::MapAccess<'de>, {
                                                #(let mut #field_idents = None;)*
                                                while let Some(key) = map.next_key()? {
                                                    match key {
                                                        #(#field_names => {
                                                            if #field_idents.is_some() {
                                                                return Err(serde::de::Error::duplicate_field(#field_names));
                                                            }
                                                            #field_idents = Some(map.next_value()?);
                                                        })*
                                                        _ => { let _: serde::de::IgnoredAny = map.next_value()?; }
                                                    }
                                                }
                                                #(let #field_idents = #field_idents.ok_or_else(|| serde::de::Error::missing_field(#field_names))?;)*
                                                Ok(#input_ident::#variant_ident(
                                                    #(#field_idents,)*
                                                ))
                                            }
                                        }

                                        serde::de::VariantAccess::struct_variant(__variant,
                                            &[
                                                #(
                                                    #field_names,
                                                )*
                                            ],
                                            __VariantVisitor {
                                                marker: core::marker::PhantomData::<#input_ident #ty_generics>,
                                            }
                                        )
                                    }
                                }
                                }
                                Tagged::Internally { .. } => {
                                    quote! {
                                    (#discriminant, __variant) => {
                                        unimplemented!()
                                    }
                                }
                                }
                                Tagged::Adjacently { .. } => {
                                    quote! {
                                    (#discriminant, __variant) => {
                                        unimplemented!()
                                    }
                                }
                                }
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            (#discriminant, __variant) => {
                                Ok(#input_ident::#variant_ident)
                            }
                        }
                    }
                }
            });

        Ok(TokenStream::from(quote! {
            impl #impl_generics serde::Deserialize<'de> for #input_ident #ty_generics #where_clause {
                fn deserialize<D>(deserializer: D) -> core::result::Result<Self, D::Error>
                where
                    D: serde::Deserializer<'de>,
                {
                    struct __Visitor #ty_generics {
                        marker: core::marker::PhantomData<#input_ident #ty_generics >,
                    }
                    impl #impl_generics serde::de::Visitor<'de> for __Visitor #ty_generics {
                        type Value = #input_ident #ty_generics;

                        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                            formatter.write_str(stringify!(enum #input_ident))
                        }

                        fn visit_enum<A>(self, data: A) -> core::result::Result<Self::Value, A::Error>
                        where
                            A: serde::de::EnumAccess<'de>,
                        {
                            struct __Discriminant(#repr_type);
                            impl __Discriminant {
                                #(#declare_discriminants)*
                            }
                            struct __DiscriminantVisitor;
                            impl<'de> serde::de::Visitor<'de> for __DiscriminantVisitor {
                                type Value = #repr_type;

                                fn expecting(&self, formatter: &mut core::fmt::Formatter) -> std::fmt::Result {
                                    formatter.write_str("identifier")
                                }

                                fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E> where E: serde::de::Error {
                                    Ok(v as #repr_type)
                                }

                                fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E> where E: serde::de::Error {
                                    let s = core::str::from_utf8(v).map_err(serde::de::Error::custom)?;
                                    s.parse().map_err(serde::de::Error::custom)
                                }

                                fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E> where E: serde::de::Error {
                                    let s = core::str::from_utf8(&v).map_err(serde::de::Error::custom)?;
                                    s.parse().map_err(serde::de::Error::custom)
                                }

                                fn visit_char<E>(self, v: char) -> Result<Self::Value, E> where E: serde::de::Error {
                                    let s = v.to_string();
                                    s.parse().map_err(serde::de::Error::custom)
                                }

                                fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> where E: serde::de::Error {
                                    v.parse().map_err(serde::de::Error::custom)
                                }

                                fn visit_string<E>(self, v: String) -> Result<Self::Value, E> where E: serde::de::Error {
                                    v.parse().map_err(serde::de::Error::custom)
                                }
                            }
                            impl<'de> serde::Deserialize<'de> for __Discriminant {
                                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> {
                                    Ok(__Discriminant(serde::Deserializer::deserialize_identifier(deserializer, __DiscriminantVisitor)?))
                                }
                            }
                            match data.variant::<__Discriminant>()? {
                                #(#match_branches)*
                                _ => {
                                    Err(serde::de::Error::custom("invalid discriminant value"))
                                }
                            }
                        }
                    }

                    serde::Deserializer::deserialize_enum(deserializer,
                        stringify!(#input_ident),
                        &[
                            #(#variant_names,)*
                        ],
                        __Visitor {
                            marker: core::marker::PhantomData::<#input_ident #ty_generics>,
                        },
                    )
                }
            }
        }))
    } else {
        let match_branches = data.variants.iter()
            .map(|variant| {
                let variant_ident = &variant.ident;
                let declare_ident = Ident::new(&format!("__{}", variant_ident.to_string().to_uppercase()), Span::call_site());
                match &variant.fields {
                    Fields::Unit => {
                        let discriminant = if other_variant.as_ref().map_or(false, |v| ((*v) as *const _) == (variant as *const _)) {
                            quote! {
                                _
                            }
                        } else {
                            quote! {
                                __Discriminant::#declare_ident
                            }
                        };
                        quote! {
                             #discriminant => {
                                Ok(#input_ident::#variant_ident)
                            }
                        }
                    }
                    _ => unreachable!("Only unit variants are expected"),
                }
            });
        let err_match = if other_variant.is_none() {
            quote! {
                _ => {
                    Err(serde::de::Error::custom("invalid discriminant value"))
                }
            }
        } else {
            quote! {}
        };
        Ok(TokenStream::from(quote! {
            impl #impl_generics serde::Deserialize<'de> for #input_ident #ty_generics #where_clause {
                fn deserialize<D>(deserializer: D) -> core::result::Result<Self, D::Error>
                where
                    D: serde::Deserializer<'de>,
                {
                    struct __Discriminant;
                    impl __Discriminant {
                        #(#declare_discriminants)*
                    }

                    match serde::Deserialize::deserialize(deserializer)? {
                        #(#match_branches)*
                        #err_match
                    }
                }
            }
        }))
    }
}

#[proc_macro_derive(Deserialize_repr, attributes(serde))]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    match derive_deserialize_impl(derive_input) {
        Ok(tokens) => tokens,
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}
