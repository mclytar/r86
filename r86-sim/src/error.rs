use std::fmt::{Display, Formatter, Debug};
use std::error::Error;

pub type SimulationResult<T> = std::result::Result<T, SimulationError>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SimulationErrorKind {
    InstructionQueueFull
}
impl SimulationErrorKind {
    pub(self) fn as_str(&self) -> &'static str {
        match *self {
            SimulationErrorKind::InstructionQueueFull => "statement queue full"
        }
    }
}

#[derive(Debug)]
struct CustomError {
    kind: SimulationErrorKind,
    error: Box<dyn Error + Send + Sync>,
}


enum ErrorSource {
    Internal(SimulationErrorKind),
    External(Box<CustomError>)
}
impl Debug for ErrorSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            ErrorSource::External(ref c) => Debug::fmt(c, f),
            ErrorSource::Internal(kind) => f.debug_tuple("Kind").field(&kind).finish()
        }
    }
}

pub struct SimulationError {
    source: ErrorSource
}
impl SimulationError {
    pub fn new<E>(kind: SimulationErrorKind, error: E) -> Self
    where
        E: Into<Box<dyn Error + Send + Sync>>
    {
        SimulationError {
            source: ErrorSource::External(Box::new(CustomError {
                kind,
                error: error.into()
            }))
        }
    }

    pub fn get_ref(&self) -> Option<&(dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::External(ref c) => Some(&*c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn get_mut(&mut self) -> Option<&mut (dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::External(ref mut c) => Some(&mut *c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn into_inner(self) -> Option<Box<dyn Error + Send + Sync>> {
        match self.source {
            ErrorSource::External(c) => Some(c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn kind(&self) -> SimulationErrorKind {
        match self.source {
            ErrorSource::External(ref c) => c.kind,
            ErrorSource::Internal(kind) => kind
        }
    }
}
impl From<SimulationErrorKind> for SimulationError {
    fn from(kind: SimulationErrorKind) -> Self {
        SimulationError {
            source: ErrorSource::Internal(kind)
        }
    }
}
impl Debug for SimulationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.source, f)
    }
}
impl Display for SimulationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.source {
            ErrorSource::External(ref c) => Display::fmt(&c.error, f),
            ErrorSource::Internal(kind) => write!(f, "{}", kind.as_str())
        }
    }
}
impl Error for SimulationError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self.source {
            ErrorSource::Internal(_) => None,
            ErrorSource::External(ref c) => c.error.source()
        }
    }
}