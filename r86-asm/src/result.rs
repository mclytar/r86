pub mod error;

pub mod prelude {
    pub use super::error::Notification;
    pub use super::error::NotificationKind;
    pub use super::error::NotificationErrorKind;
    pub use super::error::NotificationWarningKind;
    pub use super::error::Locate;
    pub use super::error::Location;
    pub use super::error::CompilerResult;
    pub use super::error::CompilerLog;
}