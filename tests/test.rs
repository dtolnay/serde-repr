use serde_repr::{Serialize_repr, Deserialize_repr};

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
