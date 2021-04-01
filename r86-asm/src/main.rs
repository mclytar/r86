use std::fs::File;
use std::io::{Read, Write};
use std::time::{Instant};

use console::style;

use r86_asm::prelude::*;
use r86_asm::linker::firmware::{FirmwareLinker};

fn get_file_contents() -> Assembly {
    let mut file = std::fs::File::open("example.asm")
        .expect("could not open `example.asm`");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("could not read `example.asm`");

    Assembly::new("<anonymous>", &contents)
}

pub fn compile(assembly: Assembly) -> Result<Binary, CompilerLog> {
    println!("  {} `{}`", style("Assembling").green().bright(), assembly.filename().to_string_lossy());
    let binary = assembly.tokenize()?
        .parse()?
        .compile()?;

    let mut linker = FirmwareLinker::new();
    linker.link(binary)?;

    Ok(linker.into_binary())
}

fn main() {
    let start = Instant::now();
    let assembly = get_file_contents();

    match compile(assembly) {
        Err(mut error_collection) => {
            error_collection.update();
            println!("{:#}", error_collection)
        },
        Ok(binary) => {
            if binary.warnings().is_warn() {
                println!("{:#}", binary.warnings());
            }

            let mut out_file = File::create("out.bin").unwrap();
            out_file.write_all(binary.as_ref()).unwrap();

            let end = Instant::now();
            let duration = end - start;
            println!("    {} raw target(s) in {}.{:02}s", style("Finished").green().bright(), duration.as_secs(), (duration.as_millis() / 10) % 100);
        }
    }
}