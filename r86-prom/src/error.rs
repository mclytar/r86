use std::fmt::{Display, Formatter, Debug};
use std::error::Error;

pub type DeviceResult<T> = std::result::Result<T, DeviceError>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DeviceErrorCode {
    EndOfFile = 0xFB,
    OutOfRange = 0xFC,
    Timeout = 0xFD,
    Unimplemented = 0xFE,
    Undefined = 0xFF,
    DoubleFault
}
impl DeviceErrorCode {
    pub(self) fn as_str(&self) -> &'static str {
        match *self {
            DeviceErrorCode::EndOfFile => "end of file",
            DeviceErrorCode::OutOfRange => "out of range",
            DeviceErrorCode::Timeout => "timeout",
            DeviceErrorCode::Unimplemented => "unimplemented",
            DeviceErrorCode::Undefined => "undefined",
            DeviceErrorCode::DoubleFault => "double fault"
        }
    }
}
impl From<u8> for DeviceErrorCode {
    fn from(code: u8) -> Self {
        match code {
            0xFB => DeviceErrorCode::EndOfFile,
            0xFC => DeviceErrorCode::OutOfRange,
            0xFD => DeviceErrorCode::Timeout,
            0xFE => DeviceErrorCode::Unimplemented,
            0xFF => DeviceErrorCode::Undefined,
            _ => DeviceErrorCode::DoubleFault
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DeviceErrorKind {
    SerialPortError,
    NotDetected,
    Timeout,
    UnexpectedResponse
}
impl DeviceErrorKind {
    pub(self) fn as_str(&self) -> &'static str {
        match *self {
            DeviceErrorKind::SerialPortError => "serial port error",
            DeviceErrorKind::NotDetected => "not detected",
            DeviceErrorKind::Timeout => "timeout",
            DeviceErrorKind::UnexpectedResponse => "unexpected response"
        }
    }
}

#[derive(Debug)]
struct CustomError {
    kind: DeviceErrorKind,
    error: Box<dyn Error + Send + Sync>,
}

enum ErrorSource {
    Device(DeviceErrorCode),
    Internal(DeviceErrorKind),
    External(Box<CustomError>)
}
impl Debug for ErrorSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            ErrorSource::Device(code) => f.debug_tuple("Error code").field(&code).finish(),
            ErrorSource::External(ref c) => Debug::fmt(c, f),
            ErrorSource::Internal(kind) => f.debug_tuple("Kind").field(&kind).finish()
        }
    }
}

pub struct DeviceError {
    source: ErrorSource
}
impl DeviceError {
    pub fn new<E>(kind: DeviceErrorKind, error: E) -> Self
        where
            E: Into<Box<dyn Error + Send + Sync>>
    {
        DeviceError {
            source: ErrorSource::External(Box::new(CustomError {
                kind,
                error: error.into()
            }))
        }
    }

    pub fn get_ref(&self) -> Option<&(dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::Device(_) => None,
            ErrorSource::External(ref c) => Some(&*c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn get_mut(&mut self) -> Option<&mut (dyn Error + Send + Sync)> {
        match self.source {
            ErrorSource::Device(_) => None,
            ErrorSource::External(ref mut c) => Some(&mut *c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn into_inner(self) -> Option<Box<dyn Error + Send + Sync>> {
        match self.source {
            ErrorSource::Device(_) => None,
            ErrorSource::External(c) => Some(c.error),
            ErrorSource::Internal(_) => None
        }
    }

    pub fn kind(&self) -> Option<DeviceErrorKind> {
        match self.source {
            ErrorSource::Device(_) => None,
            ErrorSource::External(ref c) => Some(c.kind),
            ErrorSource::Internal(kind) => Some(kind)
        }
    }

    pub fn code(&self) -> Option<DeviceErrorCode> {
        match self.source {
            ErrorSource::Device(c) => Some(c),
            _ => None
        }
    }
}
impl From<DeviceErrorKind> for DeviceError {
    fn from(kind: DeviceErrorKind) -> Self {
        DeviceError {
            source: ErrorSource::Internal(kind)
        }
    }
}
impl From<DeviceErrorCode> for DeviceError {
    fn from(code: DeviceErrorCode) -> Self {
        DeviceError {
            source: ErrorSource::Device(code)
        }
    }
}
impl Debug for DeviceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.source, f)
    }
}
impl Display for DeviceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.source {
            ErrorSource::Device(code) => write!(f, "{} (code {})", code.as_str(), code as u8),
            ErrorSource::External(ref c) => Display::fmt(&c.error, f),
            ErrorSource::Internal(kind) => write!(f, "{}", kind.as_str())
        }
    }
}
impl Error for DeviceError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self.source {
            ErrorSource::Device(_) => None,
            ErrorSource::Internal(_) => None,
            ErrorSource::External(ref c) => c.error.source()
        }
    }
}