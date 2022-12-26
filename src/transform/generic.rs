use syn::{GenericParam, Generics, TypeParamBound};

pub fn add_trait_bounds(mut generics: Generics, type_param_bound: &TypeParamBound) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(type_param_bound.clone());
        }
    }
    generics
}

pub fn add_lifetime_bounds(mut generics: Generics, lifetime: &syn::Lifetime) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Lifetime(ref mut lifetime_param) = *param {
            lifetime_param.bounds.push(lifetime.clone());
        }
    }
    generics
}
