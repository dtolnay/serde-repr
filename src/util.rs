use proc_macro2::Span;
use syn::{Field, GenericParam, Generics, Lifetime, LifetimeDef, Result, Type, TypeParam};
use syn::punctuated::Punctuated;

pub(crate) fn type_contains_generic(type_param: &TypeParam, t: &Type) -> bool {
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

pub(crate) fn type_contains_generic_lifetime(type_param: &LifetimeDef, t: &Type) -> bool {
    match t {
        Type::Path(type_path) => {
            type_path.path.segments.iter().any(|segment| {
                segment.ident == type_param.lifetime.ident
            })
        },
        Type::Reference(type_reference) => {
            if let Some(lifetime) = &type_reference.lifetime {
                lifetime.ident == type_param.lifetime.ident || type_contains_generic_lifetime(type_param, &type_reference.elem)
            } else {
                type_contains_generic_lifetime(type_param, &type_reference.elem)
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

pub(crate) fn filter_generics_by_fields<T>(generics: Generics, fields: &Punctuated<Field, T>) -> Result<Generics> {
    let mut filtered = Generics::default();
    for param in generics.params {
        match param {
            GenericParam::Type(type_param) => {
                for field in fields.iter() {
                    let ty: &Type = &field.ty;
                    if type_contains_generic(&type_param, ty) {
                        filtered.params.push(GenericParam::Type(type_param));
                        break;
                    }
                }
            }
            GenericParam::Lifetime(lifetime) => {
                for field in fields.iter() {
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

pub(crate) fn find_unused_lifetime(generics: &Generics) -> Lifetime {
    const CHARS: &str = "abcdefghijklmnopqrstuvwxyz";
    let mut i = 1;
    loop {
        let mut lifetime = String::new();
        let mut n = i;
        while n > 0 {
            lifetime.push(CHARS.chars().nth(n % CHARS.len()).unwrap());
            n /= CHARS.len();
        }

        if !generics.lifetimes().any(|l| l.lifetime.ident == lifetime) {
            return Lifetime::new(&format!("'{}", lifetime), Span::call_site());
        }
        i += 1;
    }
}
