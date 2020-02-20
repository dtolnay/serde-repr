use serde_repr::DeserializeRepr;

#[derive(DeserializeRepr)]
#[repr(u8)]
enum MultipleOthers {
    #[serde(other)]
    A,
    #[serde(other)]
    B,
}

fn main() {}
