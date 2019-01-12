#![recursion_limit = "128"]

extern crate proc_macro;

mod parse;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

use crate::parse::Input;

use std::iter;

#[proc_macro_derive(Serialize_repr)]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    let ident = input.ident;
    let repr = input.repr;
    let ident_repeat = iter::repeat(&ident);
    let variants = input.variants.iter().map(|variant| &variant.ident);
    let discriminants = input.variants.iter().map(|variant| &variant.discriminant);

    TokenStream::from(quote! {
        impl serde::Serialize for #ident {
            fn serialize<S>(&self, serializer: S) -> core::result::Result<S::Ok, S::Error>
            where
                S: serde::Serializer
            {
                let value: #repr = match *self {
                    #(
                        #ident_repeat::#variants => #discriminants,
                    )*
                };
                serde::Serialize::serialize(&value, serializer)
            }
        }
    })
}

#[proc_macro_derive(Deserialize_repr)]
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
        n => {
            "invalid value: {}, expected one of: {}".to_owned()
                + &iter::repeat(", {}").take(n - 1).collect::<String>()
        }
    };

    TokenStream::from(quote! {
        impl<'de> serde::Deserialize<'de> for #ident {
            fn deserialize<D>(deserializer: D) -> core::result::Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct discriminant;

                impl discriminant {
                    #(#declare_discriminants)*
                }

                match <#repr as serde::Deserialize>::deserialize(deserializer)? {
                    #(#match_discriminants)*
                    other => core::result::Result::Err(serde::de::Error::custom(
                        format_args!(#error_format, other #(, discriminant::#variants)*)
                    )),
                }
            }
        }
    })
}
