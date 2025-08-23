use serde_repr::Deserialize_repr;

#[derive(Deserialize_repr)]
#[repr(u8)]
enum MixedOthersUntagged {
    #[serde(untagged)]
    A,
    #[serde(other)]
    B,
}

fn main() {}
