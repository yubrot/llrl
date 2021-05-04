use super::Error;
use derive_new::new;

pub type Result<T> = std::result::Result<T, Error>;

/// The result of the entire parse process.
#[derive(PartialEq, PartialOrd, Debug, Clone, new)]
pub struct EntireResult<T> {
    pub final_result: Result<T>,
    pub inner_errors: Vec<Error>,
}

impl<T> EntireResult<T> {
    pub fn aggregate_errors(self) -> Result<T> {
        match (self.final_result, self.inner_errors.into_iter().next()) {
            // The parsing process succeeded without any errors.
            (Ok(v), None) => Ok(v),
            // There is an error occurred during the parsing process.
            (Ok(_), Some(e)) => Err(e),
            // The parsing process failed with an error.
            (Err(e), _) => Err(e),
        }
    }
}
