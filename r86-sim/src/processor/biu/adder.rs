use std::sync::Arc;

use tokio::sync::{Mutex};

use crate::bus::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum AdderControl {
    MuxM6 = -6,
    MuxM5,
    MuxM4,
    MuxM3,
    MuxM2,
    MuxM1,
    Bus,
    MuxP1,
    MuxP2,
    MuxP3,
    MuxP4,
    MuxP5,
    MuxP6
}

pub struct Adder {
    tmp_b: Arc<Mutex<u16>>,
    tmp_c: Arc<Mutex<u16>>,
    tmp_out: Arc<Mutex<u16>>,
    ctrl: Arc<Mutex<AdderControl>>
}
impl Adder {
    pub fn new() -> Self {
        Adder {
            tmp_b: Arc::new(Mutex::new(0)),
            tmp_c: Arc::new(Mutex::new(0)),
            tmp_out: Arc::new(Mutex::new(0)),
            ctrl: Arc::new(Mutex::new(AdderControl::Bus))
        }
    }

    pub fn socket_b(&self) -> Socket16 {
        Socket16::new(self.tmp_b.clone())
    }

    pub fn socket_c(&self) -> Socket20 {
        Socket20::new_high(self.tmp_c.clone())
    }

    pub fn socket_out(&self) -> Socket20 {
        Socket20::new_high(self.tmp_out.clone())
    }

    pub fn control_line(&self) -> Arc<Mutex<AdderControl>> {
        self.ctrl.clone()
    }

    pub async fn clock(&self) {
        let value = match *self.ctrl.lock().await {
            AdderControl::Bus => *self.tmp_c.lock().await,
            mux => mux as u16
        };
        *self.tmp_out.lock().await = value.overflowing_add(*self.tmp_b.lock().await).0;
    }
}



#[cfg(test)]
mod test {
    use std::sync::Arc;

    use tokio::sync::Mutex;

    use crate::processor::biu::{AdderBusB, AdderBusC};
    use crate::processor::biu::adder::{Adder, AdderControl};
    use crate::bus::{Bus, Socket20, Socket16};

    struct Setup {
        pub adder: Adder,
        pub bus_b: Bus<u16, Socket16, AdderBusB>,
        pub bus_c: Bus<u16, Socket20, AdderBusC>,
        pub reg_b: Arc<Mutex<u16>>,
        pub reg_c: Arc<Mutex<u16>>,
        pub reg_c_lo4: Arc<Mutex<u16>>,
        pub out_c: Arc<Mutex<u16>>,
        pub out_c_hi4: Arc<Mutex<u16>>
    }
    impl Setup {
        pub fn new() -> Self {
            let adder = Adder::new();
            let mut bus_b = Bus::new();
            let mut bus_c = Bus::new();

            let reg_b = Arc::new(Mutex::new(0));
            let reg_c = Arc::new(Mutex::new(0));
            let reg_c_lo4 = Arc::new(Mutex::new(0));
            let out_c = Arc::new(Mutex::new(0));
            let out_c_hi4 = Arc::new(Mutex::new(0));

            bus_b.plug(AdderBusB::ALU, adder.socket_b());
            bus_b.plug(AdderBusB::RegFile, Socket16::new(reg_b.clone()));

            bus_c.plug(AdderBusC::ALU, adder.socket_c());
            bus_c.plug(AdderBusC::ALUOut, adder.socket_out());
            bus_c.plug(AdderBusC::RegFile, Socket20::new_high(reg_c.clone()));
            bus_c.plug(AdderBusC::RegFileLo4, Socket20::new_low_nibble(reg_c_lo4.clone()));
            bus_c.plug(AdderBusC::Out, Socket20::new_low(out_c.clone()));
            bus_c.plug(AdderBusC::OutHi4, Socket20::new_high_nibble(out_c_hi4.clone()));

            Setup {
                adder,
                bus_b,
                bus_c,
                reg_b,
                reg_c,
                reg_c_lo4,
                out_c,
                out_c_hi4
            }
        }
    }

    #[tokio::test]
    async fn adder_output() {
        let mut setup = Setup::new();

        *setup.reg_b.lock().await = 0x1234;
        *setup.reg_c.lock().await = 0x5678;
        *setup.reg_c_lo4.lock().await = 0x9ABC;

        setup.bus_b[AdderBusB::ALU].input();
        setup.bus_b[AdderBusB::RegFile].output();
        setup.bus_c[AdderBusC::ALU].input();
        setup.bus_c[AdderBusC::RegFile].output();
        setup.bus_c[AdderBusC::RegFileLo4].output();
        setup.bus_c[AdderBusC::Out].input();
        setup.bus_c[AdderBusC::OutHi4].input();
        setup.bus_b.clock().await.unwrap();
        setup.bus_c.clock().await.unwrap();
        setup.adder.clock().await;
        assert_eq!(*setup.out_c.lock().await, 0x678C);
        assert_eq!(*setup.out_c_hi4.lock().await, 0x5);
    }

    #[tokio::test]
    async fn adder_sum() {
        let mut setup = Setup::new();

        *setup.reg_b.lock().await = 0x1234;
        *setup.reg_c.lock().await = 0x5678;
        *setup.reg_c_lo4.lock().await = 0x9ABC;

        setup.bus_b[AdderBusB::ALU].input();
        setup.bus_b[AdderBusB::RegFile].output();
        setup.bus_c[AdderBusC::ALU].input();
        setup.bus_c[AdderBusC::RegFile].output();
        setup.bus_c[AdderBusC::RegFileLo4].output();
        setup.bus_c[AdderBusC::Out].input();
        setup.bus_c[AdderBusC::OutHi4].input();
        setup.bus_b.clock().await.unwrap();
        setup.bus_c.clock().await.unwrap();
        setup.adder.clock().await;

        setup.bus_c.close_all();
        setup.bus_c[AdderBusC::ALUOut].output();
        setup.bus_c[AdderBusC::RegFile].input();
        setup.adder.clock().await;
        setup.bus_c.clock().await.unwrap();
        assert_eq!(*setup.reg_c.lock().await, 0x68AC);
    }

    #[tokio::test]
    async fn adder_inc() {
        let mut setup = Setup::new();

        *setup.reg_b.lock().await = 0x7C00;

        setup.bus_b[AdderBusB::ALU].input();
        setup.bus_b[AdderBusB::RegFile].output();
        setup.bus_c[AdderBusC::ALUOut].output();
        setup.bus_c[AdderBusC::RegFile].input();

        *setup.adder.control_line().lock().await = AdderControl::MuxP2;
        setup.bus_b.clock().await.unwrap();
        setup.adder.clock().await;
        setup.bus_c.clock().await.unwrap();

        assert_eq!(*setup.reg_c.lock().await, 0x7C02);
    }

    #[tokio::test]
    async fn adder_dec() {
        let mut setup = Setup::new();

        *setup.reg_b.lock().await = 0x7C00;

        setup.bus_b[AdderBusB::ALU].input();
        setup.bus_b[AdderBusB::RegFile].output();
        setup.bus_c[AdderBusC::ALUOut].output();
        setup.bus_c[AdderBusC::RegFile].input();

        *setup.adder.control_line().lock().await = AdderControl::MuxM2;
        setup.bus_b.clock().await.unwrap();
        setup.adder.clock().await;
        setup.bus_c.clock().await.unwrap();

        assert_eq!(*setup.reg_c.lock().await, 0x7BFE);
    }

    #[tokio::test]
    async fn adder_inc_overflow() {
        let mut setup = Setup::new();

        *setup.reg_b.lock().await = 0xFFFE;

        setup.bus_b[AdderBusB::ALU].input();
        setup.bus_b[AdderBusB::RegFile].output();
        setup.bus_c[AdderBusC::ALUOut].output();
        setup.bus_c[AdderBusC::RegFile].input();

        *setup.adder.control_line().lock().await = AdderControl::MuxP4;
        setup.bus_b.clock().await.unwrap();
        setup.adder.clock().await;
        setup.bus_c.clock().await.unwrap();

        assert_eq!(*setup.reg_c.lock().await, 0x0002);
    }

    #[tokio::test]
    async fn adder_dec_overflow() {
        let mut setup = Setup::new();

        *setup.reg_b.lock().await = 0x0002;

        setup.bus_b[AdderBusB::ALU].input();
        setup.bus_b[AdderBusB::RegFile].output();
        setup.bus_c[AdderBusC::ALUOut].output();
        setup.bus_c[AdderBusC::RegFile].input();

        *setup.adder.control_line().lock().await = AdderControl::MuxM4;
        setup.bus_b.clock().await.unwrap();
        setup.adder.clock().await;
        setup.bus_c.clock().await.unwrap();

        assert_eq!(*setup.reg_c.lock().await, 0xFFFE);
    }
}