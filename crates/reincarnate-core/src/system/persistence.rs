use serde::{de::DeserializeOwned, Serialize};

/// Persistence system trait — handles save/load storage.
///
/// Uses serde for serialization, allowing backends to choose
/// the storage mechanism (filesystem, localStorage, cloud, etc.).
pub trait Persistence {
    type Error: std::error::Error;

    fn save<T: Serialize>(&mut self, slot: &str, data: &T) -> Result<(), Self::Error>;
    fn load<T: DeserializeOwned>(&self, slot: &str) -> Result<Option<T>, Self::Error>;
    fn delete(&mut self, slot: &str) -> Result<(), Self::Error>;
    fn list_slots(&self) -> Result<Vec<String>, Self::Error>;
}
