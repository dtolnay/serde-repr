use serde_repr::SerializeRepr;

#[derive(SerializeRepr)]
struct SmallPrime {
    two: u8,
    three: u8,
    five: u8,
    seven: u8,
}

fn main() {}
