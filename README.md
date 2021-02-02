# r86

Helpers for designing and building an 8086 computer

## Purpose

I am designing and building a motherboard for an Intel 8086 microprocessor,
therefore I need additional software to help me reach this goal.

## Installation

Currently, the only installation method is by using cargo:

```shell
cargo install --path x86-asm
cargo install --path x86-prom
cargo install --path x86-sim
```

For the programmer, you need either Visual Studio Code with the [PlatformIO](https://github.com/platformio/platformio-core) extension
or the Arduino IDE; also, you need an Arduino MEGA 2560.
To install, simply compile and upload the sketch to Arduino.

## Description

### `./r86-asm`

This will be the main assembler & disassembler, which will allow also to split the generated file into two pieces (even address and odd address);
this is necessary for programming the ROM since the 8086 needs two 8-bit ROMs.

Yeah, I know, assemblers already exist (MASM, NASM, TASM, etc.), but I need some extra functionalities;
also, I will probably need to construct a compiler too, so an assembler seems a good starting point.

ℹ️ _I have currently no plans for this sub-project._

### `./r86-prom`

This is the EEPROM programmer, along with the Arduino programmer `./programmer`.
The only currently supported EEPROM is the AT28C256, but I might add other EEPROMs if needed.

In particular, `./r86-prom` is a command-line interface that allows to communicate with the Arduino programmer `./programmer`
using byte op-codes; on the other side, `./programmer` receives and executes the incoming commands (e.g. read, write, ping, ...).

ℹ️ _At the moment, the application works, but it is only able to execute simple commands such as "read everything", "write everything" or "test if EEPROM is good".
Also, the source code is a bit of a mess._

### `./r86-sim`

This should be a simulator for the 8086 system, which accurately simulates the processor and its integration with the other components.

ℹ️ _I have currently no plans for this sub-project._

### `./machine`

This subdirectory contains all the source codes and binaries for the actual system, such as ROM, operating system, etc.

## License

[MIT License](LICENSE.md)