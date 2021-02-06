use std::sync::Arc;

use tokio::sync::{Mutex};

pub struct BusSocket<T, S> {
    pub(self) contents: Arc<Mutex<T>>,
    pub(self) reader: Box<dyn Fn(T) -> S + Send + Sync>,
    pub(self) writer: Box<dyn Fn(T, S) -> T + Send + Sync>
}
impl<T, S> BusSocket<T, S> where
    T: Copy
{
    pub async fn read(&self) -> S {
        let v = *self.contents.lock().await;
        (self.reader)(v)
    }

    pub async fn write(&self, v: S) {
        let old_v = *self.contents.lock().await;
        let v = (self.writer)(old_v, v);
        *self.contents.lock().await = v
    }
}

pub trait Bus {
    type Contents: Copy;

    fn contents(&self) -> Arc<Mutex<Self::Contents>>;
    fn make_socket<S>(&self, reader: Box<dyn Fn(Self::Contents) -> S + Send + Sync>, writer: Box<dyn Fn(Self::Contents, S) -> Self::Contents + Send + Sync>) -> BusSocket<Self::Contents, S> {
        BusSocket {
            contents: self.contents(),
            reader,
            writer
        }
    }
    fn socket(&self) -> BusSocket<Self::Contents, Self::Contents> {
        let reader = Box::new(|v| v);
        let writer = Box::new(|_, v| v);
        self.make_socket(reader, writer)
    }
}

pub struct Bus8 {
    contents: Arc<Mutex<u8>>
}
impl Bus8 {
    pub fn new() -> Self {
        Bus8 {
            contents: Arc::new(Mutex::new(0))
        }
    }
}
impl Bus for Bus8 {
    type Contents = u8;

    fn contents(&self) -> Arc<Mutex<Self::Contents>> {
        self.contents.clone()
    }
}

pub struct Bus16 {
    contents: Arc<Mutex<u16>>
}
impl Bus16 {
    pub fn new() -> Self {
        Bus16 {
            contents: Arc::new(Mutex::new(0))
        }
    }
    pub fn socket_low8(&self) -> BusSocket<u16, u8> {
        let reader = Box::new(|v| (v & 0xFF) as u8);
        let writer = Box::new(|old_v, v| (old_v & 0xFF00) | (v as u16));
        self.make_socket(reader, writer)
    }
    pub fn socket_high8(&self) -> BusSocket<u16, u8> {
        let reader = Box::new(|v| ((v & 0xFF00) >> 8) as u8);
        let writer = Box::new(|old_v, v| (old_v & 0x00FF) | ((v as u16) << 8));
        self.make_socket(reader, writer)
    }
}
impl Bus for Bus16 {
    type Contents = u16;

    fn contents(&self) -> Arc<Mutex<Self::Contents>> {
        self.contents.clone()
    }
}

pub struct Bus20 {
    contents: Arc<Mutex<u32>>
}
impl Bus20 {
    pub fn new() -> Self {
        Bus20 {
            contents: Arc::new(Mutex::new(0))
        }
    }
    pub fn socket_low16(&self) -> BusSocket<u32, u16> {
        let reader = Box::new(|v| (v & 0x0FFFF) as u16);
        let writer = Box::new(|old_v, v| (old_v & 0xF0000) | (v as u32));
        self.make_socket(reader, writer)
    }
    pub fn socket_high16(&self) -> BusSocket<u32, u16> {
        let reader = Box::new(|v| ((v & 0xFFFF0) >> 4) as u16);
        let writer = Box::new(|old_v, v| (old_v & 0x0000F) | ((v as u32) << 4));
        self.make_socket(reader, writer)
    }
    pub fn socket_low8(&self) -> BusSocket<u32, u8> {
        let reader = Box::new(|v| (v & 0xFF) as u8);
        let writer = Box::new(|old_v, v| (old_v & 0xFFF00) | (v as u32));
        self.make_socket(reader, writer)
    }
    pub fn socket_high8(&self) -> BusSocket<u32, u8> {
        let reader = Box::new(|v| ((v & 0xFF000) >> 12) as u8);
        let writer = Box::new(|old_v, v| (old_v & 0x00FFF) | ((v as u32) << 12));
        self.make_socket(reader, writer)
    }
    pub fn socket_low4(&self) -> BusSocket<u32, u8> {
        let reader = Box::new(|v| (v & 0x0F) as u8);
        let writer = Box::new(|old_v, v| (old_v & 0xFFFF0) | (v as u32));
        self.make_socket(reader, writer)
    }
    pub fn socket_high4(&self) -> BusSocket<u32, u8> {
        let reader = Box::new(|v| ((v & 0xF0000) >> 16) as u8);
        let writer = Box::new(|old_v, v| (old_v & 0x0FFFF) | (((v & 0xF) as u32) << 16));
        self.make_socket(reader, writer)
    }
}
impl Bus for Bus20 {
    type Contents = u32;

    fn contents(&self) -> Arc<Mutex<Self::Contents>> {
        self.contents.clone()
    }
}

#[cfg(test)]
mod test {
    use crate::bus::{Bus8, Bus, Bus16, Bus20};

    #[tokio::test]
    async fn test_bus8() {
        let bus8 = Bus8::new();
        let socket1 = bus8.socket();
        let socket2 = bus8.socket();

        socket1.write(0x55).await;
        assert_eq!(socket2.read().await, 0x55);
    }

    #[tokio::test]
    async fn test_bus16() {
        let bus16 = Bus16::new();
        let out_socket = bus16.socket();
        let in_socket = bus16.socket();
        let socket_low = bus16.socket_low8();
        let socket_high = bus16.socket_high8();

        out_socket.write(0x55AA).await;
        assert_eq!(in_socket.read().await, 0x55AA);
        assert_eq!(socket_low.read().await, 0xAA);
        assert_eq!(socket_high.read().await, 0x55);

        socket_low.write(0x00).await;
        assert_eq!(in_socket.read().await, 0x5500);
        assert_eq!(socket_low.read().await, 0x00);
        assert_eq!(socket_high.read().await, 0x55);

        socket_high.write(0x7C).await;
        assert_eq!(in_socket.read().await, 0x7C00);
        assert_eq!(socket_low.read().await, 0x00);
        assert_eq!(socket_high.read().await, 0x7C);
    }

    #[tokio::test]
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
    }
}