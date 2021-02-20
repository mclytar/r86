//! Bus for allowing multiple components to communicate.

use std::sync::Arc;

use tokio::sync::{Mutex};

use crate::error::{SimulationResult, SimulationError, SimulationErrorKind};
use std::marker::PhantomData;
use std::convert::{TryFrom, TryInto};
use std::ops::{BitAndAssign, BitOrAssign, ShlAssign, ShrAssign, Index, IndexMut};
use std::collections::{HashMap};
use std::hash::Hash;

/// Data direction of the socket.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SignalDirection {
    /// The socket is isolated from the bus.
    ThreeState,
    /// The socket updates the reference from the data on the bus.
    Input,
    /// The socket puts the value behind the reference on the bus.
    Output
}

/// This structure keeps track of the active lines of the bus and the respective content.
#[derive(Clone)]
pub struct Signal {
    out_lanes: u32,
    in_lanes: u32,
    value: u32
}
impl Signal {
    /// Creates a new, empty signal.
    pub fn new() -> Self {
        Signal {
            out_lanes: 0,
            in_lanes: 0,
            value: 0
        }
    }
    /// Creates a new signal carrying the specified `value`.
    pub fn with_u8(value: u8) -> Self {
        Signal {
            out_lanes: 0xFF,
            in_lanes: 0,
            value: value as u32
        }
    }
    /// Creates a new signal carrying the specified `value`.
    pub fn with_u16(value: u16) -> Self {
        Signal {
            out_lanes: 0xFFFF,
            in_lanes: 0,
            value: value as u32
        }
    }
    /// Swaps the low byte and the high byte of the low 16 bits of the signal.
    pub fn swap_u16(&mut self) {
        let lanes = self.out_lanes as u16;
        self.out_lanes &= 0xFFFF0000;
        self.out_lanes |= lanes.swap_bytes() as u32;

        let value = self.value as u16;
        self.value &= 0xFFFF0000;
        self.value |= value.swap_bytes() as u32;
    }
    /// Enables input lanes on the signal.
    pub fn input_enable(&mut self, lanes: u32) -> SimulationResult<()> {
        if lanes & !self.out_lanes != 0 {
            Err(SimulationError::from(SimulationErrorKind::BusEmpty))
        } else {
            self.in_lanes |= lanes;
            Ok(())
        }
    }
    /// Applies the preconfigured input lanes to the specified `value`.
    pub fn apply<T>(&mut self, value: T) -> T where
        T: Default + TryFrom<u32> + TryInto<u32> {
        let mut value = value.try_into().unwrap_or(0);
        value &= !self.in_lanes;
        value |= self.value & self.in_lanes;
        value.try_into().unwrap_or(Default::default())
    }
    /// Adds the output lines of `other` to this signal.
    pub fn join(&mut self, other: Self) -> SimulationResult<()> {
        if self.out_lanes & other.out_lanes != 0 {
            Err(SimulationError::from(SimulationErrorKind::BusRaceCondition))
        } else {
            self.out_lanes |= other.out_lanes;
            self.value |= other.out_lanes & other.value;
            Ok(())
        }
    }
}
impl BitAndAssign<u32> for Signal {
    fn bitand_assign(&mut self, rhs: u32) {
        self.out_lanes &= rhs;
        self.value &= rhs;
    }
}
impl BitOrAssign<u32> for Signal {
    fn bitor_assign(&mut self, rhs: u32) {
        self.out_lanes |= rhs;
        self.value |= rhs;
    }
}
impl ShlAssign<usize> for Signal {
    fn shl_assign(&mut self, rhs: usize) {
        self.out_lanes <<= rhs;
        self.value <<= rhs;
    }
}
impl ShrAssign<usize> for Signal {
    fn shr_assign(&mut self, rhs: usize) {
        self.out_lanes >>= rhs;
        self.value >>= rhs;
    }
}

/// An interface for dealing with bus sockets.
///
/// This contains the main functions to transform and interface with a value that is attached to the bus.
pub trait Socket<T: Copy> {
    /// Maps a value into a bus signal.
    fn output_map(&self, value: T) -> Signal;
    /// Updates a value using a bus signal.
    fn input_map(&self, value: T, bg: Signal) -> SimulationResult<T>;
    /// Obtains a multi-thread-secure reference to the value behind the socket.
    fn value(&self) -> Arc<Mutex<T>>;
    /// Isolates the socket from the bus.
    fn close(&mut self) -> &mut Self;
}

/// A bus which allows several components to communicate.
pub struct Bus<T, S, I> where
    T: Copy,
    S: Socket<T>,
    I: Eq + Hash
{
    sockets: HashMap<I, S>,
    _phantom: PhantomData<T>
}
impl<T, S, I> Bus<T, S, I> where
    T: Copy,
    S: Socket<T>,
    I: Eq + Hash
{
    /// Creates a new, empty bus.
    pub fn new() -> Self {
        Bus {
            sockets: HashMap::new(),
            _phantom: PhantomData::default()
        }
    }

    /// Attaches a socket to the bus.
    pub fn plug(&mut self, index: I, socket: S) {
        self.sockets.insert(index, socket);
    }

    /// Puts all the associated sockets in high-z state.
    pub fn close_all(&mut self) {
        self.sockets.iter_mut()
            .for_each(|(_, s)| { s.close(); });
    }

    /// Puts the socket relative to the value pointed by `pointer` in high-z state.
    pub fn close(&mut self, pointer: Arc<Mutex<T>>) {
        self.sockets.iter_mut()
            .filter(|(_, s)| &*s.value() as * const _ == &*pointer as * const _)
            .for_each(|(_, s)| { s.close(); });
    }

    /// Obtains a reference to the selected socket, if any.
    pub fn get(&self, index: I) -> Option<&S> {
        self.sockets.get(&index)
    }

    /// Obtains a mutable reference to the selected socket, if any.
    pub fn get_mut(&mut self, index: I) -> Option<&mut S> {
        self.sockets.get_mut(&index)
    }

    /// Executes one bus clock cycle, thus copying all the output values into the input values.
    pub async fn clock(&mut self) -> SimulationResult<()> {
        let mut signal = Signal::new();

        for (_, s) in self.sockets.iter() {
            let value = s.value();
            let inner = value.lock().await;
            signal.join(s.output_map(*inner))?;
        }

        for (_, s) in self.sockets.iter() {
            let value = s.value();
            let mut inner = value.lock().await;
            *inner = s.input_map(*inner, signal.clone())?;
        }

        Ok(())
    }
}
impl<T, S, I> Index<I> for Bus<T, S, I> where
    T: Copy,
    S: Socket<T>,
    I: Eq + Hash
{
    type Output = S;

    fn index(&self, index: I) -> &Self::Output {
        self.sockets.get(&index).unwrap()
    }
}
impl<T, S, I> IndexMut<I> for Bus<T, S, I> where
    T: Copy,
    S: Socket<T>,
    I: Eq + Hash
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.sockets.get_mut(&index).unwrap()
    }
}

/// 16-bit socket for communication through a [`Bus`].
pub struct Socket16 {
    value: Arc<Mutex<u16>>,
    lo8: SignalDirection,
    hi8: SignalDirection,
    cross: bool
}
impl Socket16 {
    /// Creates a new socket for the specified reference.
    pub fn new(value: Arc<Mutex<u16>>) -> Self {
        Socket16 {
            value,
            lo8: SignalDirection::ThreeState,
            hi8: SignalDirection::ThreeState,
            cross: false
        }
    }
    /// Configures the socket to output the whole referenced value.
    pub fn output(&mut self) -> &mut Self {
        self.lo8 = SignalDirection::Output;
        self.hi8 = SignalDirection::Output;
        self
    }
    /// Configures the socket to output the low byte of the referenced value.
    pub fn output_lo8(&mut self) -> &mut Self {
        self.lo8 = SignalDirection::Output;
        self
    }
    /// Configures the socket to output the high byte of the referenced value.
    pub fn output_hi8(&mut self) -> &mut Self{
        self.hi8 = SignalDirection::Output;
        self
    }
    /// Configures the socket to update the whole referenced value from the input.
    pub fn input(&mut self) -> &mut Self {
        self.lo8 = SignalDirection::Input;
        self.hi8 = SignalDirection::Input;
        self
    }
    /// Configures the socket to update the low byte of the referenced value from the input.
    pub fn input_lo8(&mut self) -> &mut Self {
        self.lo8 = SignalDirection::Input;
        self
    }
    /// Configures the socket to update the high byte of the referenced value from the input.
    pub fn input_hi8(&mut self) -> &mut Self {
        self.hi8 = SignalDirection::Input;
        self
    }
    /// Isolates the low byte of the referenced value from the bus.
    pub fn close_lo8(&mut self) -> &mut Self {
        self.lo8 = SignalDirection::ThreeState;
        self
    }
    /// Isolates the high byte of the referenced value from the bus.
    pub fn close_hi8(&mut self) -> &mut Self {
        self.lo8 = SignalDirection::ThreeState;
        self
    }
    /// Enables the cross circuit, which switches the low byte with the high byte.
    pub fn set_cross(&mut self) -> &mut Self {
        self.cross = true;
        self
    }
    /// Disables the cross circuit, which switches the low byte with the high byte.
    pub fn clear_cross(&mut self) -> &mut Self {
        self.cross = false;
        self
    }
}
impl Socket<u16> for Socket16 {
    fn output_map(&self, value: u16) -> Signal {
        let mut guard = Signal::with_u16(value);
        if self.hi8 != SignalDirection::Output { guard &= 0x00FF; }
        if self.lo8 != SignalDirection::Output { guard &= 0xFF00; }
        if self.cross { guard.swap_u16(); }
        guard
    }

    fn input_map(&self, value: u16, mut guard: Signal) -> SimulationResult<u16> {
        if self.cross { guard.swap_u16(); }
        if self.lo8 == SignalDirection::Input { guard.input_enable(0x00FF)?; }
        if self.hi8 == SignalDirection::Input { guard.input_enable(0xFF00)?; }
        Ok(guard.apply(value))
    }

    fn value(&self) -> Arc<Mutex<u16>> {
        self.value.clone()
    }

    fn close(&mut self) -> &mut Self {
        self.lo8 = SignalDirection::ThreeState;
        self.hi8 = SignalDirection::ThreeState;
        self
    }
}

/// 20-bit socket for communication through a [`Bus`].
pub struct Socket20 {
    value: Arc<Mutex<u16>>,
    direction: SignalDirection,
    high: bool,
    nibble: bool
}
impl Socket20 {
    /// Creates a new socket for the specified reference, plugging it to the lower 16 bits of the bus.
    pub fn new_low(value: Arc<Mutex<u16>>) -> Self {
        Socket20 {
            value,
            direction: SignalDirection::ThreeState,
            high: false,
            nibble: false
        }
    }
    /// Creates a new socket for the specified reference, plugging it to the higher 16 bits of the bus.
    pub fn new_high(value: Arc<Mutex<u16>>) -> Self {
        Socket20 {
            value,
            direction: SignalDirection::ThreeState,
            high: true,
            nibble: false
        }
    }
    /// Creates a new socket for the specified reference, plugging it to the lower 4 bits of the bus.
    pub fn new_low_nibble(value: Arc<Mutex<u16>>) -> Self {
        Socket20 {
            value,
            direction: SignalDirection::ThreeState,
            high: false,
            nibble: true
        }
    }
    /// Creates a new socket for the specified reference, plugging it to the higher 4 bits of the bus.
    pub fn new_high_nibble(value: Arc<Mutex<u16>>) -> Self {
        Socket20 {
            value,
            direction: SignalDirection::ThreeState,
            high: true,
            nibble: true
        }
    }
    /// Configures the socket to output the referenced value.
    pub fn output(&mut self) {
        self.direction = SignalDirection::Output;
    }
    /// Configures the socket to update the whole referenced value from the input.
    pub fn input(&mut self) {
        self.direction = SignalDirection::Input;
    }
}
impl Socket<u16> for Socket20 {
    fn output_map(&self, value: u16) -> Signal {
        if self.direction != SignalDirection::Output { return Signal::new(); }
        let mut guard = Signal::with_u16(value);
        if self.nibble { guard &= 0xF; }
        if self.high { guard <<= if self.nibble { 16 } else { 4 }; }
        guard
    }

    fn input_map(&self, value: u16, mut guard: Signal) -> SimulationResult<u16> {
        if self.direction != SignalDirection::Input { return Ok(0); }
        if self.high { guard >>= if self.nibble { 16 } else { 4 }; }
        if self.nibble {
            guard.input_enable(0xF)?;
        } else {
            guard.input_enable(0xFFFF)?;
        }
        Ok(guard.apply(value))
    }

    fn value(&self) -> Arc<Mutex<u16>> {
        self.value.clone()
    }

    fn close(&mut self) -> &mut Self {
        self.direction = SignalDirection::ThreeState;
        self
    }
}



#[cfg(test)]
mod test {
    use crate::bus::{Bus, Socket16, Socket, Socket20};

    use std::sync::Arc;
    use tokio::sync::Mutex;

    #[derive(Copy, Clone, Eq, PartialEq, Hash)]
    enum TestReg16 {
        A,
        B
    }
    #[derive(Copy, Clone, Eq, PartialEq, Hash)]
    enum TestReg20 {
        A,
        B,
        C,
        D
    }

    #[tokio::test]
    async fn test_bus16() {
        let mut bus = Bus::new();
        let reg16_a = Arc::new(Mutex::new(0));
        let reg16_b = Arc::new(Mutex::new(0));
        bus.plug(TestReg16::A, Socket16::new(reg16_a.clone()));
        bus.plug(TestReg16::B, Socket16::new(reg16_b.clone()));

        // Test 16 ---> 16
        bus[TestReg16::A]
            .output();
        bus[TestReg16::B]
            .input();
        *reg16_a.lock().await = 0x55AA;
        assert_eq!(*reg16_b.lock().await, 0);
        bus.clock().await.unwrap();
        assert_eq!(*reg16_b.lock().await, 0x55AA);

        // Test 16 -X-> 16
        bus[TestReg16::B]
            .set_cross();
        bus.clock().await.unwrap();
        assert_eq!(*reg16_b.lock().await, 0xAA55);
        bus[TestReg16::B]
            .clear_cross();

        // Test l8 ---> l8
        bus.close_all();
        bus[TestReg16::A]
            .output_lo8();
        bus[TestReg16::B]
            .input_lo8();
        *reg16_a.lock().await = 0x00FF;
        bus.clock().await.unwrap();
        assert_eq!(*reg16_b.lock().await, 0xAAFF);

        // Test l8 -X-> h8
        bus[TestReg16::B]
            .set_cross();
        bus[TestReg16::B]
            .close()
            .input_hi8();
        *reg16_a.lock().await = 0xCCEE;
        bus.clock().await.unwrap();
        assert_eq!(*reg16_b.lock().await, 0xEEFF);

        // Test h8 -X-> l8
        bus.close_all();
        bus[TestReg16::A]
            .set_cross()
            .output_hi8();
        bus[TestReg16::B]
            .clear_cross()
            .input_lo8();
        *reg16_a.lock().await = 0xDD11;
        bus.clock().await.unwrap();
        assert_eq!(*reg16_b.lock().await, 0xEEDD);

        // Test h8 ---> h8
        bus.close_all();
        bus[TestReg16::A]
            .clear_cross()
            .output_hi8();
        bus[TestReg16::B]
            .input_hi8();
        *reg16_a.lock().await = 0x9876;
        bus.clock().await.unwrap();
        assert_eq!(*reg16_b.lock().await, 0x98DD);
    }

    #[tokio::test]
    async fn test_bus20() {
        let mut bus = Bus::new();
        let reg16_a = Arc::new(Mutex::new(0));
        let reg16_b = Arc::new(Mutex::new(0));
        let reg16_c = Arc::new(Mutex::new(0));
        let reg16_d = Arc::new(Mutex::new(0));
        bus.plug(TestReg20::A, Socket20::new_low(reg16_a.clone()));
        bus.plug(TestReg20::B, Socket20::new_low_nibble(reg16_b.clone()));
        bus.plug(TestReg20::C, Socket20::new_high(reg16_c.clone()));
        bus.plug(TestReg20::D, Socket20::new_high_nibble(reg16_d.clone()));

        *reg16_c.lock().await = 0xFEDC;
        *reg16_b.lock().await = 0xB;
        bus[TestReg20::B]
            .output();
        bus[TestReg20::C]
            .output();
        bus[TestReg20::A]
            .input();
        bus[TestReg20::D]
            .input();
        bus.clock().await.unwrap();
        assert_eq!(*reg16_d.lock().await & 0xF, 0xF);
        assert_eq!(*reg16_a.lock().await, 0xEDCB);
    }

    /*#[tokio::test]
    async fn test_bus20() {
        let bus20 = Bus20::new();
        let out_socket = bus20.socket();
        let in_socket = bus20.socket();
        let socket_low4 = bus20.socket_low4();
        let socket_high4 = bus20.socket_high4();
        let socket_low8 = bus20.socket_low8();
        let socket_high8 = bus20.socket_high8();
        let socket_low16 = bus20.socket_low16();
        let socket_high16 = bus20.socket_high16();

        out_socket.write(0x12345).await;
        assert_eq!(in_socket.read().await, 0x12345);
        assert_eq!(socket_low4.read().await, 0x5);
        assert_eq!(socket_high4.read().await, 0x1);
        assert_eq!(socket_low8.read().await, 0x45);
        assert_eq!(socket_high8.read().await, 0x12);
        assert_eq!(socket_low16.read().await, 0x2345);
        assert_eq!(socket_high16.read().await, 0x1234);

        socket_low4.write(0xF).await;
        assert_eq!(in_socket.read().await, 0x1234F);

        socket_high4.write(0xE).await;
        assert_eq!(in_socket.read().await, 0xE234F);

        socket_low8.write(0xDC).await;
        assert_eq!(in_socket.read().await, 0xE23DC);

        socket_high8.write(0xBA).await;
        assert_eq!(in_socket.read().await, 0xBA3DC);

        socket_low16.write(0x9876).await;
        assert_eq!(in_socket.read().await, 0xB9876);

        socket_high16.write(0x5432).await;
        assert_eq!(in_socket.read().await, 0x54326);
    }*/
}