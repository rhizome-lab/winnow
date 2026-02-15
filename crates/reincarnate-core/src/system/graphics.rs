/// RGBA color.
#[derive(Debug, Clone, Copy)]
pub struct Color {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

/// Axis-aligned rectangle.
#[derive(Debug, Clone, Copy)]
pub struct Rect {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}

/// Graphics system trait — handles all visual output (2D drawing primitives).
///
/// Associated types allow backends to use their own texture/surface representations
/// (e.g., `wgpu::Texture` for native, `HtmlCanvasElement` for web).
pub trait Graphics {
    type Texture;
    type Surface;

    fn clear(&mut self, color: Color);
    fn draw_sprite(&mut self, texture: &Self::Texture, x: f32, y: f32);
    fn draw_sprite_scaled(
        &mut self,
        texture: &Self::Texture,
        x: f32,
        y: f32,
        scale_x: f32,
        scale_y: f32,
    );
    fn draw_sprite_rotated(
        &mut self,
        texture: &Self::Texture,
        x: f32,
        y: f32,
        angle: f32,
    );
    fn draw_rect(&mut self, rect: Rect, color: Color);
    fn draw_text(&mut self, text: &str, x: f32, y: f32, size: f32);
    fn load_texture(&mut self, data: &[u8]) -> Self::Texture;
    fn surface_width(&self) -> u32;
    fn surface_height(&self) -> u32;
    fn present(&mut self);
}
