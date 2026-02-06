/// Keyboard key identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Key {
    A, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
    Num0, Num1, Num2, Num3, Num4, Num5, Num6, Num7, Num8, Num9,
    F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
    Up, Down, Left, Right,
    Space, Enter, Escape, Tab, Backspace, Delete,
    Shift, Ctrl, Alt,
}

/// Mouse button identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
}

/// Input system trait — handles keyboard, mouse, and touch input.
pub trait Input {
    fn is_key_down(&self, key: Key) -> bool;
    fn is_key_pressed(&self, key: Key) -> bool;
    fn is_key_released(&self, key: Key) -> bool;

    fn is_mouse_down(&self, button: MouseButton) -> bool;
    fn is_mouse_pressed(&self, button: MouseButton) -> bool;
    fn is_mouse_released(&self, button: MouseButton) -> bool;
    fn mouse_x(&self) -> f32;
    fn mouse_y(&self) -> f32;

    fn update(&mut self);
}
