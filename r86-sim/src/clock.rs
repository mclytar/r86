use crate::error::SimulationResult;

pub trait Clock {
    fn clock_rise(&mut self) -> SimulationResult<()> { Ok(()) }
    fn clock_high(&mut self) -> SimulationResult<()> { Ok(()) }
    fn clock_fall(&mut self) -> SimulationResult<()> { Ok(()) }
    fn clock_low(&mut self) -> SimulationResult<()> { Ok(()) }
}