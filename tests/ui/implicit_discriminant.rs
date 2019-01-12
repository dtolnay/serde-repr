use serde_repr::Serialize_repr;

#[derive(Serialize_repr)]
#[repr(u8)]
enum SmallPrime {
    Two,
    Three,
    Five,
    Seven,
}

fn main() {}
