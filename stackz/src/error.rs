use std::fmt::Display;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Fatal error: {0}")]
    Fatal(String),
}

impl Error {
    pub fn fatal(message: impl Display) -> Self {
        Self::Fatal(message.to_string())
    }

    pub fn fatal_with(message: impl FnOnce() -> String) -> Self {
        Self::Fatal(message())
    }
}

pub trait ErrorExt<T> {
    fn fatal(self, message: impl Display) -> Result<T>;

    fn fatal_with(self, message: impl FnOnce() -> String) -> Result<T>;
}

impl<T> ErrorExt<T> for Option<T> {
    fn fatal(self, message: impl Display) -> Result<T> {
        self.ok_or_else(|| Error::Fatal(message.to_string()))
    }

    fn fatal_with(self, message: impl FnOnce() -> String) -> Result<T> {
        self.ok_or_else(|| Error::Fatal(message()))
    }
}

impl<T, E> ErrorExt<T> for std::result::Result<T, E>
where
    E: Display,
{
    fn fatal(self, message: impl Display) -> Result<T> {
        self.map_err(|error| Error::Fatal(format!("{}: {}", message, error)))
    }

    fn fatal_with(self, message: impl FnOnce() -> String) -> Result<T> {
        self.map_err(|error| Error::Fatal(format!("{}: {}", message(), error)))
    }
}
