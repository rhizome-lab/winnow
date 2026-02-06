/// Timing system trait — handles frame pacing and time tracking.
pub trait Timing {
    /// Seconds elapsed since last frame.
    fn delta_time(&self) -> f64;

    /// Total seconds elapsed since start.
    fn elapsed(&self) -> f64;

    /// Current frame number.
    fn frame_count(&self) -> u64;

    /// Target frames per second (0 = uncapped).
    fn target_fps(&self) -> u32;

    /// Set target frames per second.
    fn set_target_fps(&mut self, fps: u32);

    /// Called once per frame to advance timing state.
    fn tick(&mut self);
}
