/// Audio system trait — handles sound and music playback.
pub trait Audio {
    type Sound;
    type Channel;

    fn load_sound(&mut self, data: &[u8]) -> Self::Sound;
    fn play(&mut self, sound: &Self::Sound) -> Self::Channel;
    fn play_looped(&mut self, sound: &Self::Sound) -> Self::Channel;
    fn stop(&mut self, channel: &Self::Channel);
    fn set_volume(&mut self, channel: &Self::Channel, volume: f32);
    fn is_playing(&self, channel: &Self::Channel) -> bool;
    fn stop_all(&mut self);
}
