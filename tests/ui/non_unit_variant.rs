use serde_repr::Serialize_repr;

#[derive(Serialize_repr)]
#[repr(u8)]
#[serde(tag = "type")]
enum SmallPrime {
    Two(u8),
    Three(u8),
    Five(u8),
    Seven(u8),
}

fn main() {}
