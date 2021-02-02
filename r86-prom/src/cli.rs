pub mod utils;

pub use clap::Clap;
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum WriteMode {
    Strict,
    Any,
    Offset(u16)
}

pub enum Mode {
    Read {
        filename: Option<PathBuf>
    },
    Write {
        filename: Option<PathBuf>,
        write_mode: WriteMode
    },
    Test,
    Interactive
}

/// Controls the AT28C256 EEPROM programmer.
#[derive(Clap)]
pub struct Options {
    /// Verifies that the chip works as intended.
    ///
    /// WARNING: this procedure completely erases the contents of the ROM.
    /// It is advised to run the command `--dump -o <file>` to perform a backup.
    #[clap(long)]
    test: bool,
    /// Reads the entire contents of the ROM and displays it, or saves it into a file if the
    /// `--output` flag is specified.
    #[clap(long)]
    dump: bool,
    /// When performing a `--dump`, specifies a destination file for the contents.
    #[clap(short, long, requires("dump"))]
    output: Option<PathBuf>,
    /// Writes the contents of a file into the ROM.
    ///
    /// The file must be specified with `--input`.
    ///
    /// The file must be exactly 32768 bytes long, unless either `--allow-any` or `--offset`
    /// are specified.
    #[clap(long)]
    program: bool,
    /// When performing a `--program`, specifies a source file for the contents.
    #[clap(short, long, requires("program"))]
    input: Option<PathBuf>,
    /// Allows files of any size to be written.
    ///
    /// NOTE: if the size of the file is greater than 32768 bytes, only the first 32768 bytes will
    /// be written
    #[clap(long, requires("program"))]
    allow_any: bool,
    /// Allows files of any size to be written, starting at the specified address.
    ///
    /// NOTE: the file will be clipped at 32768 bytes minus the offset.
    #[clap(long, requires("program"))]
    offset: Option<u16>,
    /// Interactive mode.
    ///
    /// Allows to continuously send commands and receive bytes.
    #[clap(long)]
    interactive: bool,
}

impl Options {
    pub fn from_command_line() -> Self {
        Self::parse()
    }

    pub fn into_mode(self) -> Mode {
        if self.test {
            Mode::Test
        } else if self.interactive {
            Mode::Interactive
        } else if self.dump {
            Mode::Read { filename: self.output }
        } else if self.program {
            Mode::Write {
                filename: self.input,
                write_mode: if self.allow_any {
                    WriteMode::Any
                } else if let Some(offset) = self.offset {
                    WriteMode::Offset(offset)
                } else {
                    WriteMode::Strict
                }
            }
        } else {
            Mode::Interactive
        }
    }
}