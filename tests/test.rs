#![allow(
    clippy::derive_partial_eq_without_eq,
    // Clippy bug: https://github.com/rust-lang/rust-clippy/issues/7422
    clippy::nonstandard_macro_braces,
    clippy::wildcard_imports,
)]

use serde_repr::{Deserialize_repr, Serialize_repr};

mod small_prime {
    use super::*;

    #[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug)]
    #[repr(u8)]
    enum SmallPrime {
        Two = 2,
        Three = 3,
        Five = 5,
        Seven = 7,
    }

    #[test]
    fn test_serialize() {
        let j = serde_json::to_string(&SmallPrime::Seven).unwrap();
        assert_eq!(j, "7");
    }

    #[test]
    fn test_deserialize() {
        let p: SmallPrime = serde_json::from_str("2").unwrap();
        assert_eq!(p, SmallPrime::Two);
    }
}

mod other {
    use super::*;

    #[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug)]
    #[repr(u8)]
    enum TestOther {
        A,
        B,
        #[serde(other, rename = "useless")]
        Other,
    }

    #[test]
    fn test_deserialize() {
        let p: TestOther = serde_json::from_str("0").unwrap();
        assert_eq!(p, TestOther::A);
        let p: TestOther = serde_json::from_str("1").unwrap();
        assert_eq!(p, TestOther::B);
        let p: TestOther = serde_json::from_str("5").unwrap();
        assert_eq!(p, TestOther::Other);
    }
}

mod implicit_discriminant {
    use super::*;

    #[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug)]
    #[repr(u8)]
    enum ImplicitDiscriminant {
        Zero,
        One,
        Two,
        Three,
    }

    #[test]
    fn test_serialize() {
        let j = serde_json::to_string(&ImplicitDiscriminant::Three).unwrap();
        assert_eq!(j, "3");
    }

    #[test]
    fn test_deserialize() {
        let p: ImplicitDiscriminant = serde_json::from_str("2").unwrap();
        assert_eq!(p, ImplicitDiscriminant::Two);
    }
}

mod externally_tagged_complex_enum {
    use super::*;

    const SOME_CONST: u8 = 12;

    #[derive(Serialize_repr, Deserialize_repr, Debug, PartialEq)]
    #[repr(u8)]
    enum ComplexEnum<'b, T> {
        A { a: u8, b: u16 } = SOME_CONST,
        B { b: T },
        C(u8, u16, u32) = 16,
        D(&'b str),
        E,
        F,
    }

    #[test]
    fn test_serialize() {
        let j = serde_json::to_string(&ComplexEnum::B { b: ComplexEnum::<&str>::C(1, 2, 3) }).unwrap();
        assert_eq!(j, "{\"13\":{\"b\":{\"16\":[1,2,3]}}}");
    }

    #[test]
    fn test_deserialize() {
        let p: ComplexEnum<ComplexEnum<&str>> = serde_json::from_str("{\"13\":{\"b\":{\"16\":[1,2,3]}}}").unwrap();
        assert_eq!(p, ComplexEnum::B { b: ComplexEnum::<&str>::C(1, 2, 3) });
    }
}