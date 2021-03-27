use r86_asm::result::error::{CompilerLog};
use console::style;
use std::time::{Instant};
use r86_asm::lexer::Assembly;

fn get_file_contents() -> Assembly {
    let contents = r##"
    main:
                                    mov     ax, dx
                                    xor     cx, cx
                                    org     0
                                    dec     cx
                                    GLOBAL  main
    .L0:                            loop    .L0
            TIMES 0xFFF0 - ($ - $$) nop
    reset:
                                    jmp     main
            TIMES 10 - ($ - reset)  db      0x00
    version:
                                    dw      0x00
        .major:                     dw      0x01
        .minor:                     dw      0x00
            "##;

    Assembly::new("<anonymous>", contents)
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