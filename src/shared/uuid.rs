//! # UUID
//!
//! Version 4 UUIDs, which unfog mints for the `UID` of an item
//! composed from nothing.

use anyhow::{Result, anyhow};

/// Mints a fresh random version 4 UUID, in the canonical hyphenated form.
pub fn uuid_v4() -> Result<String> {
    let mut bytes = [0u8; 16];
    getrandom::fill(&mut bytes).map_err(|err| anyhow!("Gather randomness error: {err}"))?;

    // NOTE: RFC 4122 4.4 stamps version 4 and variant 10xx.
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    bytes[8] = (bytes[8] & 0x3f) | 0x80;

    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = [0u8; 36];
    let mut cursor = 0;

    for (i, byte) in bytes.iter().enumerate() {
        if matches!(i, 4 | 6 | 8 | 10) {
            out[cursor] = b'-';
            cursor += 1;
        }

        out[cursor] = HEX[(byte >> 4) as usize];
        out[cursor + 1] = HEX[(byte & 0x0f) as usize];
        cursor += 2;
    }

    Ok(String::from_utf8(out.to_vec()).expect("ASCII hex is always valid UTF-8"))
}

#[cfg(test)]
mod tests {
    use super::uuid_v4;

    #[test]
    fn a_minted_uuid_has_the_canonical_shape() {
        let uuid = uuid_v4().unwrap();

        assert_eq!(uuid.len(), 36);
        assert_eq!(
            uuid.char_indices()
                .filter(|(_, c)| *c == '-')
                .map(|(i, _)| i)
                .collect::<Vec<_>>(),
            vec![8, 13, 18, 23],
        );

        // RFC 4122 4.4: the version nibble is 4 and the variant one is 8, 9,
        // a or b.
        assert_eq!(&uuid[14..15], "4");
        assert!(matches!(&uuid[19..20], "8" | "9" | "a" | "b"));
    }

    #[test]
    fn two_minted_uuids_differ() {
        assert_ne!(uuid_v4().unwrap(), uuid_v4().unwrap());
    }
}
