/// Save UI system trait — handles save/load slot presentation.
pub trait SaveUi {
    /// Show the save/load UI with available slots.
    fn show(&mut self);

    /// Close the save/load UI.
    fn close(&mut self);
}
