use serde_repr::Serialize_repr;

#[derive(Serialize_repr)]
#[repr(u8)]
enum MultipleOthers {
    #[serde(other)]
    A,
    #[serde(other)]
    B,
}

fn main() {}
