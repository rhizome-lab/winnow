/// Images system trait — handles bitmap/texture loading from binary data.
pub trait Images {
    type Image;
    type Error: std::error::Error;

    /// Load an image from raw binary data (PNG, JPEG, etc.).
    fn load_image(&self, data: &[u8]) -> Result<Self::Image, Self::Error>;
}
