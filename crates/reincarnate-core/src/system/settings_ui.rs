/// Settings UI system trait — handles settings/preferences presentation.
pub trait SettingsUi {
    /// Show the settings UI.
    fn show(&mut self);

    /// Close the settings UI.
    fn close(&mut self);
}
