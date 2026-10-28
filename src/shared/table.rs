//! # Table style
//!
//! The `comfy_table` preset mapper shared by every listing.
//!
//! `comfy_table` v8 dropped the positional preset string in favour of a typed
//! [`TableStyle`] builder. The `table.preset` config option keeps accepting
//! the v7 string, so a configuration written against unfog 0.1 stays
//! valid; this module maps one onto the other.

use pimalaya_cli::table::{ContentLineStyle, LineStyle, TableStyle};

/// The default preset, `comfy_table` v7's `UTF8_FULL_CONDENSED`: full UTF-8
/// borders with no divider between rows.
pub const DEFAULT_PRESET: &str = "││──╞═╪╡┆    ┬┴┌┐└┘";

/// How many table components a preset string can style.
const COMPONENTS: usize = 19;

/// Maps a `comfy_table` v7 positional preset string onto a [`TableStyle`].
///
/// Each character styles one component, in the order of the v7
/// `TableComponent` enum:
///
/// ```text
///  0 left border            10 middle intersections
///  1 right border           11 left border intersections
///  2 top border             12 right border intersections
///  3 bottom border          13 top border intersections
///  4 left header inters.    14 bottom border intersections
///  5 header lines           15 top left corner
///  6 middle header inters.  16 top right corner
///  7 right header inters.   17 bottom left corner
///  8 vertical lines         18 bottom right corner
///  9 horizontal lines
/// ```
///
/// A space leaves a component undrawn, and so does one a short string leaves
/// out, both matching v7 where an unset component rendered blank. Characters
/// past the nineteenth are ignored.
pub fn style_from_preset(preset: &str) -> TableStyle {
    let mut chars = [None; COMPONENTS];

    for (slot, char) in chars.iter_mut().zip(preset.chars()) {
        *slot = (char != ' ').then_some(char);
    }

    TableStyle::new()
        .top_border(LineStyle {
            left: chars[15],
            fill: chars[2],
            junction: chars[13],
            right: chars[16],
        })
        .header_lines(ContentLineStyle {
            left: chars[0],
            junction: chars[8],
            right: chars[1],
        })
        .header_separator(LineStyle {
            left: chars[4],
            fill: chars[5],
            junction: chars[6],
            right: chars[7],
        })
        .content_lines(ContentLineStyle {
            left: chars[0],
            junction: chars[8],
            right: chars[1],
        })
        .row_separator(LineStyle {
            left: chars[11],
            fill: chars[9],
            junction: chars[10],
            right: chars[12],
        })
        .bottom_border(LineStyle {
            left: chars[17],
            fill: chars[3],
            junction: chars[14],
            right: chars[18],
        })
}

#[cfg(test)]
mod tests {
    use pimalaya_cli::table::presets;

    use super::{DEFAULT_PRESET, style_from_preset};

    // NOTE: equality against the v8 constants that replaced the v7 preset
    // strings, across all six line styles, is what proves the
    // character-to-builder-slot mapping.

    #[test]
    fn the_default_preset_is_utf8_full_condensed() {
        assert_eq!(
            style_from_preset(DEFAULT_PRESET),
            presets::UTF8_FULL_CONDENSED
        );
    }

    #[test]
    fn the_bordered_presets_map_onto_their_upstream_constants() {
        assert_eq!(style_from_preset("││──╞═╪╡┆╌┼├┤┬┴┌┐└┘"), presets::UTF8_FULL);
        assert_eq!(
            style_from_preset("||--+==+|-+||++++++"),
            presets::ASCII_FULL
        );
    }

    #[test]
    fn a_short_or_spaced_preset_leaves_its_components_undrawn() {
        assert_eq!(
            style_from_preset("||  |-|||           "),
            presets::ASCII_MARKDOWN
        );
        assert_eq!(
            style_from_preset("     ═╪ ┆╌┼        "),
            presets::UTF8_NO_BORDERS
        );
        assert_eq!(style_from_preset(""), presets::NOTHING);
    }
}
