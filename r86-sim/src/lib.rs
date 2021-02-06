pub mod clock;
pub mod error;

pub mod processor;
pub mod bus;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Signal {
    Low,
    High
}