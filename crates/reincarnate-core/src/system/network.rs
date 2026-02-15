/// Network system trait — handles HTTP requests and resource fetching.
pub trait Network {
    type Error: std::error::Error;
    type Response;

    /// Fetch a resource by URL.
    fn fetch(&self, url: &str) -> Result<Self::Response, Self::Error>;

    /// Check whether network fetching is available.
    fn is_available(&self) -> bool;
}
