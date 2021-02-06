macro_rules! bit_enum {
    ($(#[$meta:meta])* $vis:vis enum $name:ident : $bitmask:literal >> $shift:literal {
        $($(#[$v_meta:meta])* $v_name:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$v_meta])* $v_name $(= $val)?,)*
        }
        impl $name {
            fn decode(val: u32) -> Option<$name> {
                match ((val & $bitmask) >> $shift) {
                    $(x if x == $name::$v_name as u32 => Some($name::$v_name),)*
                    _ => None,
                }
            }
        }
    }
}

bit_enum! {
    #[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
    pub enum MicrocodeSource : 0b000001111100000000000 >> 11 {
        ProgramCounter          = 0b00010,
        BIURegisterOpr          = 0b00011,
        BIURegisterCS           = 0b00100,
        BIURegisterInd          = 0b00110,
        PrefetchQueue           = 0b00111,
        RegisterAL              = 0b01000,
        TempA                   = 0b01010,
        TempC                   = 0b01011,
        TempB                   = 0b01110,
        Flags                   = 0b01111,
        RegisterAH              = 0b10000,
        OpcodeOperand           = 0b10001,
        Sigma                   = 0b10010,
        UnknownCR               = 0b10011,
        OpcodeRegister          = 0b10101,
        Zero                    = 0b10111,
        RegisterAX              = 0b11000,
        RegisterDX              = 0b11001,
        RegisterSP              = 0b11010,
        RegisterSI              = 0b11011,
        RegisterCX              = 0b11100,
        RegisterBX              = 0b11101,
        RegisterBP              = 0b11110,
        RegisterDI              = 0b11111,
    }
}

bit_enum! {
    #[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
    pub enum MicrocodeDestination : 0b111110000000000000000 >> 16 {
        BIURegisterES           = 0b00000,
        RegisterAH              = 0b00001,
        RegisterAL              = 0b00010,
        RegisterAX              = 0b00011,
        ProgramCounter          = 0b00100,
        TempALow                = 0b00101,
        TempA                   = 0b00110,
        RegisterSP              = 0b00111,
        BIURegisterSS           = 0b01000,
        OpcodeOperand           = 0b01001,
        RegisterDX              = 0b01011,
        BIURegisterOpr          = 0b01100,
        TempAHigh               = 0b01101,
        TempC                   = 0b01110,
        RegisterSI              = 0b01111,
        BIURegisterCS           = 0b10000,
        RegisterCX              = 0b10011,
        BIURegisterInd          = 0b10100,
        TempBLow                = 0b10101,
        TempB                   = 0b10110,
        BIURegisterDS           = 0b11000,
        OpcodeRegister          = 0b11001,
        TempBHigh               = 0b11101,
        Flags                   = 0b11110,
        RegisterDI              = 0b11111,
    }
}

bit_enum! {
    #[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
    pub enum MicrocodeLongJumpCondition : 0b0001110000 >> 4 {
        Unconditional           = 0b000,
        NoF1Flag                = 0b001,
        NonZero                 = 0b010,
        Bit3IsOne               = 0b011,
        NoCarry                 = 0b100,
        F1Flag                  = 0b101,
        InterruptPending        = 0b110,
        OpcodeCondition         = 0b111,
    }
}

bit_enum! {
    #[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
    pub enum MicrocodeShortJumpCondition  : 0b0011110000 >> 4 {
        RepNEFlagDiffersFromZF  = 0b0000,
        EffectiveAddressIsOdd   = 0b0001,
        ImmediateIsByte         = 0b0010,
        ZeroFlag                = 0b0011,
        InternalCounterNotZero  = 0b0100,
        TestPinNotAsserted      = 0b0101,
        OverflowFlag            = 0b0110,
        CarryFlag               = 0b0111,
        Unconditional           = 0b1000,
        NoF1Flag                = 0b1001,
        NonZero                 = 0b1010,
        Bit3IsZero              = 0b1011,
        NoCarry                 = 0b1100,
        F1Flag                  = 0b1101,
        InterruptPending        = 0b1110,
        OpcodeCondition         = 0b1111,
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum MicrocodeInstructionType {
    ShortJump(MicrocodeShortJumpCondition, u8),
    ALUOperation,
    Miscellaneous,
    LongJump(MicrocodeLongJumpCondition, u8),
    BusOperation,
    LongCall(MicrocodeLongJumpCondition, u8)
}
impl MicrocodeInstructionType {
    pub fn decode(op: u32) -> Self {
        match (op & 0b1110000000) >> 7 {
            0b000
            | 0b001 => MicrocodeInstructionType::ShortJump(MicrocodeShortJumpCondition::decode(op).unwrap(), (op & 0b1111) as u8),
            0b010
            | 0b011 => MicrocodeInstructionType::ALUOperation,
            0b100 => MicrocodeInstructionType::Miscellaneous,
            0b101 => MicrocodeInstructionType::LongJump(MicrocodeLongJumpCondition::decode(op).unwrap(), (op & 0b1111) as u8),
            0b110 => MicrocodeInstructionType::BusOperation,
            0b111 => MicrocodeInstructionType::LongCall(MicrocodeLongJumpCondition::decode(op).unwrap(), (op & 0b1111) as u8),
            _ => unreachable!()
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct MicrocodeOperation {
    destination: Option<MicrocodeDestination>,
    source: Option<MicrocodeSource>,
    affect_flags: bool,
    instruction_type: MicrocodeInstructionType
}
impl MicrocodeOperation {
    pub fn decode(op: u32) -> Self {
        let destination = MicrocodeDestination::decode(op);
        let source = MicrocodeSource::decode(op);
        let affect_flags = op & 0b000000000010000000000 != 0;
        let instruction_type = MicrocodeInstructionType::decode(op);

        MicrocodeOperation {
            destination,
            source,
            affect_flags,
            instruction_type
        }
    }
}