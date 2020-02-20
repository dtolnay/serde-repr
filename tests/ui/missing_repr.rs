use serde_repr::SerializeRepr;

#[derive(SerializeRepr)]
enum SmallPrime {
    Two = 2,
    Three = 3,
    Five = 5,
    Seven = 7,
}

fn main() {}
