use r86_asm::result::error::{CompilerLog};
use console::style;
use std::time::{Instant};
use r86_asm::lexer::Assembly;
use std::io::Read;

fn get_file_contents() -> Assembly {
    let mut file = std::fs::File::open("example.asm")
        .expect("could not open `example.asm`");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("could not read `example.asm`");

    Assembly::new("<anonymous>", &contents)
}

pub fn compile(assembly: Assembly) -> Result<(), CompilerLog> {
    println!("  {} `{}`", style("Assembling").green().bright(), assembly.filename().to_string_lossy());
    let _listing = assembly.tokenize()?
        .parse()?
        .compile()?;

    Ok(())
}

fn main() {
    let start = Instant::now();
    let assembly = get_file_contents();

    match compile(assembly) {
        Err(mut error_collection) => {
            error_collection.update();
            println!("{:#}", error_collection)
        },
        Ok(_) => {
            /*let mut out_file = File::create("out.bin").unwrap();
            out_file.write_all(&assembly.to_raw()[..]).unwrap();*/

            let end = Instant::now();
            let duration = end - start;
            println!("    {} raw target(s) in {}.{:02}s", style("Finished").green().bright(), duration.as_secs(), (duration.as_millis() / 10) % 100);
        }
    }
}