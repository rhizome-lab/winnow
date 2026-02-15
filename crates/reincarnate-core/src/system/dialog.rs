/// Dialog system trait — handles modal dialogue boxes and alerts.
pub trait Dialog {
    /// Show a text message (dialogue box, alert, etc.).
    fn show_message(&mut self, text: &str);

    /// Show a message with choices, returning the selected index.
    fn show_choices(&mut self, prompt: &str, choices: &[&str]) -> usize;

    /// Show a text input prompt, returning the entered text.
    fn show_text_input(&mut self, prompt: &str, default: &str) -> String;

    /// Show a numeric input prompt.
    fn show_number_input(&mut self, prompt: &str, default: f64, min: f64, max: f64) -> f64;

    /// Close the current dialog.
    fn close(&mut self);

    /// Check whether a dialog is currently open.
    fn is_open(&self) -> bool;
}
