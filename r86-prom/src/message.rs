use crate::error::{DeviceErrorCode, DeviceResult, DeviceError};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DeviceRequest {
    None,
    ChipEnable,
    ChipDisable,
    QueryAddress,
    SelectAddress(u16),
    Ping,
    ReadByte,
    ReadByteAt(u16),
    ReadPage(u16),
    ReadAll,
    WriteByte(u8),
    WriteByteAt(u16, u8),
    WritePage(u16),
    WriteAll,
    ProtectedWriteByte(u8),
    ProtectedWriteByteAt(u16, u8),
    ProtectedWritePage(u16),
    ProtectedWriteAll,
    UnprotectAndWriteByte(u8),
    UnprotectAndWriteByteAt(u16, u8),
    UnprotectAndWritePage(u16),
    UnprotectAndWriteAll,
}
impl DeviceRequest {
    pub fn opcode(&self) -> u8 {
        match *self {
            DeviceRequest::None => 0x00,
            DeviceRequest::ChipEnable => 0x01,
            DeviceRequest::ChipDisable => 0x02,
            DeviceRequest::QueryAddress => 0x03,
            DeviceRequest::SelectAddress(_) => 0x04,
            DeviceRequest::Ping => 0x05,
            DeviceRequest::ReadByte => 0x10,
            DeviceRequest::ReadByteAt(_) => 0x11,
            DeviceRequest::ReadPage(_) => 0x12,
            DeviceRequest::ReadAll => 0x13,
            DeviceRequest::WriteByte(_) => 0x20,
            DeviceRequest::WriteByteAt(_, _) => 0x21,
            DeviceRequest::WritePage(_) => 0x22,
            DeviceRequest::WriteAll => 0x23,
            DeviceRequest::ProtectedWriteByte(_) => 0x24,
            DeviceRequest::ProtectedWriteByteAt(_, _) => 0x25,
            DeviceRequest::ProtectedWritePage(_) => 0x26,
            DeviceRequest::ProtectedWriteAll => 0x27,
            DeviceRequest::UnprotectAndWriteByte(_) => 0x28,
            DeviceRequest::UnprotectAndWriteByteAt(_, _) => 0x29,
            DeviceRequest::UnprotectAndWritePage(_) => 0x2A,
            DeviceRequest::UnprotectAndWriteAll => 0x2B
        }
    }

    pub fn decode(&self) -> Vec<u8> {
        match *self {
            DeviceRequest::SelectAddress(data)
            | DeviceRequest::ReadByteAt(data)
            | DeviceRequest::ReadPage(data)
            | DeviceRequest::WritePage(data)
            | DeviceRequest::ProtectedWritePage(data)
            | DeviceRequest::UnprotectAndWritePage(data) => vec![self.opcode(), (data & 0xFF) as u8, (data >> 8) as u8],
            DeviceRequest::WriteByte(byte)
            | DeviceRequest::ProtectedWriteByte(byte)
            | DeviceRequest::UnprotectAndWriteByte(byte) => vec![self.opcode(), byte],
            DeviceRequest::WriteByteAt(addr, byte)
            | DeviceRequest::ProtectedWriteByteAt(addr, byte)
            | DeviceRequest::UnprotectAndWriteByteAt(addr, byte) => vec![self.opcode(), (addr & 0xFF) as u8, (addr >> 8) as u8, byte],
            _ => vec![self.opcode()]
        }
    }

    pub fn needs_payload(&self) -> bool {
        match *self {
            DeviceRequest::WritePage(_)
            | DeviceRequest::WriteAll
            | DeviceRequest::ProtectedWritePage(_)
            | DeviceRequest::ProtectedWriteAll
            | DeviceRequest::UnprotectAndWritePage(_)
            | DeviceRequest::UnprotectAndWriteAll
            => true,
            _ => false
        }
    }

    pub fn needs_buffer(&self) -> bool {
        match *self {
            DeviceRequest::ReadPage(_)
            | DeviceRequest::ReadAll
            => true,
            _ => false
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DeviceResponse {
    Ok,
    Multipart,
    Ready
}
impl DeviceResponse {
    pub fn from_response(response: u8) -> DeviceResult<DeviceResponse> {
        match response {
            0x00 => Ok(DeviceResponse::Ok),
            0x01 => Ok(DeviceResponse::Multipart),
            0x02 => Ok(DeviceResponse::Ready),
            _ => Err(DeviceError::from(DeviceErrorCode::from(response)))
        }
    }
}