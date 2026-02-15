/// Layout system trait — handles page frame, sidebar, and chrome rendering.
pub trait Layout {
    /// Render the sidebar/navigation frame.
    fn render_sidebar(&mut self);

    /// Hide the sidebar, preserving layout offset.
    fn stow_sidebar(&mut self);

    /// Show a previously hidden sidebar.
    fn unstow_sidebar(&mut self);

    /// Remove the sidebar entirely.
    fn destroy_sidebar(&mut self);
}
