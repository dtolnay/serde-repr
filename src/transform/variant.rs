use proc_macro2::{Ident, Span};
use syn::{Attribute, DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed, GenericParam, Lifetime, LifetimeDef, Result, Type, TypeReference, Variant, Visibility};
use syn::punctuated::Punctuated;
use syn::token::Colon;

use crate::util::filter_generics_by_fields;

pub fn struct_from_variant(
    derive_input: &DeriveInput,
    variant: &Variant,
    lifetime: Option<&Lifetime>,
    attrs: Vec<Attribute>,
) -> Result<DeriveInput> {
    let fields = variant.fields.iter().map(|field| {
        Field {
            attrs: field.attrs.iter()
                .filter(|attr| attr.path.is_ident("serde"))
                .map(|attr| attr.clone())
                .collect(),
            vis: Visibility::Inherited,
            ident: field.ident.clone(),
            colon_token: Some(Colon::default()),
            ty: if let Some(lifetime) = lifetime {
                Type::Reference(TypeReference {
                    and_token: Default::default(),
                    lifetime: Some(lifetime.clone()),
                    mutability: None,
                    elem: Box::new(field.ty.clone()),
                })
            } else {
                field.ty.clone()
            },
        }
    }).collect::<Punctuated<_, _>>();
    let mut generics = filter_generics_by_fields(derive_input.generics.clone(), &fields)?;
    if let Some(lifetime) = lifetime {
        generics.params.push(GenericParam::Lifetime(LifetimeDef {
            attrs: vec![],
            lifetime: lifetime.clone(),
            colon_token: None,
            bounds: Punctuated::new(),
        }));
    }

    Ok(DeriveInput {
        ident: Ident::new("__AdjacentlyTagged", Span::call_site()),
        attrs,
        generics,
        data: syn::Data::Struct(syn::DataStruct {
            fields: match &variant.fields {
                Fields::Named(f) => Fields::Named(FieldsNamed {
                    named: fields,
                    brace_token: f.brace_token.clone(),
                }),
                Fields::Unnamed(f) => Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields,
                    paren_token: f.paren_token.clone(),
                }),
                Fields::Unit => Fields::Unit,
            },
            struct_token: syn::token::Struct::default(),
            semi_token: Some(syn::token::Semi::default()),
        }),
        vis: Visibility::Inherited,
    })
}
