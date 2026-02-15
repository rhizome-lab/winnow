/// Files system trait — handles file download/write operations.
pub trait Files {
    type Error: std::error::Error;

    /// Trigger a file download or write with the given filename and data.
    fn download(&self, filename: &str, data: &[u8]) -> Result<(), Self::Error>;
}
