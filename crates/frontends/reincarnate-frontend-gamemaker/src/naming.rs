//! Naming convention utilities for converting GML identifiers to idiomatic TypeScript.

/// Convert `snake_case` to `PascalCase`.
///
/// ```text
/// "button_base" → "ButtonBase"
/// "dice"        → "Dice"
/// ```
pub fn snake_to_pascal(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = true;
    for ch in s.chars() {
        if ch == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(ch.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }
    result
}

/// Convert `snake_case` to `camelCase`.
///
/// ```text
/// "button_click" → "buttonClick"
/// "dice"         → "dice"
/// ```
pub fn snake_to_camel(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = false;
    let mut first = true;
    for ch in s.chars() {
        if ch == '_' {
            if first {
                // Leading underscores: preserve them as-is.
                result.push('_');
            } else {
                capitalize_next = true;
            }
        } else if capitalize_next {
            result.push(ch.to_ascii_uppercase());
            capitalize_next = false;
            first = false;
        } else {
            result.push(ch);
            first = false;
        }
    }
    result
}

/// Convert a GML object name to PascalCase, stripping the `obj_` prefix.
///
/// ```text
/// "obj_button_base" → "ButtonBase"
/// "obj_dice"        → "Dice"
/// "Player"          → "Player"    (no prefix → PascalCase as-is)
/// ```
pub fn object_name_to_pascal(name: &str) -> String {
    let stripped = name.strip_prefix("obj_").unwrap_or(name);
    snake_to_pascal(stripped)
}

/// Convert a GML sprite name to PascalCase, stripping the `spr_` prefix.
///
/// ```text
/// "spr_player" → "Player"
/// "spr_button_base" → "ButtonBase"
/// ```
pub fn sprite_name_to_pascal(name: &str) -> String {
    let stripped = name.strip_prefix("spr_").unwrap_or(name);
    snake_to_pascal(stripped)
}

/// Convert a GML room name to PascalCase, stripping the `rm_` prefix.
///
/// ```text
/// "rm_init"    → "Init"
/// "rm_battle"  → "Battle"
/// ```
pub fn room_name_to_pascal(name: &str) -> String {
    let stripped = name.strip_prefix("rm_").unwrap_or(name);
    snake_to_pascal(stripped)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snake_to_pascal() {
        assert_eq!(snake_to_pascal("button_base"), "ButtonBase");
        assert_eq!(snake_to_pascal("dice"), "Dice");
        assert_eq!(snake_to_pascal("a_b_c"), "ABC");
        assert_eq!(snake_to_pascal("already"), "Already");
        assert_eq!(snake_to_pascal(""), "");
    }

    #[test]
    fn test_snake_to_camel() {
        assert_eq!(snake_to_camel("button_click"), "buttonClick");
        assert_eq!(snake_to_camel("dice"), "dice");
        assert_eq!(snake_to_camel("a_b_c"), "aBC");
        assert_eq!(snake_to_camel("_private"), "_private");
        assert_eq!(snake_to_camel(""), "");
    }

    #[test]
    fn test_object_name_to_pascal() {
        assert_eq!(object_name_to_pascal("obj_button_base"), "ButtonBase");
        assert_eq!(object_name_to_pascal("obj_dice"), "Dice");
        assert_eq!(object_name_to_pascal("Player"), "Player");
        assert_eq!(object_name_to_pascal("obj_a"), "A");
    }

    #[test]
    fn test_sprite_name_to_pascal() {
        assert_eq!(sprite_name_to_pascal("spr_player"), "Player");
        assert_eq!(sprite_name_to_pascal("spr_button_base"), "ButtonBase");
        assert_eq!(sprite_name_to_pascal("custom"), "Custom");
    }

    #[test]
    fn test_room_name_to_pascal() {
        assert_eq!(room_name_to_pascal("rm_init"), "Init");
        assert_eq!(room_name_to_pascal("rm_battle"), "Battle");
        assert_eq!(room_name_to_pascal("lobby"), "Lobby");
    }
}
