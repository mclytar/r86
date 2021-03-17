use console::{style};

//use r86_asm::lexer::{Lexer};
use r86_asm::error::{CompilerError};
use r86_asm::token::TokenStream;
use r86_asm::statement::Statement;

fn get_file_contents() -> String {
    /*let mut asm_file = File::open("source.asm").unwrap();
    let mut contents = String::new();
    asm_file.read_to_string(&mut contents).unwrap();*/

    let contents = r##"
    main:
            nop
            add     ax, cx
            sub     al, ah
            add     WORD [es: bx + si + (MyStruct + 2) + 8], 0x80   ; some comment
            inc     ch
            inc     dx      ; some other comment
    .L0:    nop
    Another_Label:
            aaa
    loop:   xor     ax, ax
            jmp     loop"##.to_owned();

    contents
}

fn display_errors(contents: &str, token_err: &[CompilerError]) {
    let err_count = token_err.len();

    for err in token_err {
        let line = err.line().unwrap();
        let column = err.column().unwrap();

        let contents = contents.lines()
            .skip(line - 1)
            .next()
            .unwrap();

        let len = err.length().unwrap_or(1);

        let description = err.description();

        println!("{}: {}", style("error").red().bright(), description);
        println!(" {} {}:{}:{}", style("-->").cyan().bright(), "filename", line, column);
        println!("{} {}", style(line).hidden(), style("|").cyan().bright());
        println!("{} {} {}", style(line).cyan().bright(), style("|").cyan().bright(), contents);
        let caption = if description.starts_with("expected ") {
            if let Some(pos) = description.find(", found") {
                &description[..pos]
            } else { "" }
        } else { "" };
        let help = if let Some(hint) = err.help() {
            format!("help: {}", hint)
        } else {
            String::new()
        };
        println!("{} {}{}{} {}{}", style(line).hidden(), style("|").cyan().bright(), " ".repeat(column), style("^".repeat(len)).red().bright(), style(caption).red().bright(), style(help).red().bright());
        println!();
    }

    if err_count > 1 {
        println!("{}: aborting due to {} previous errors", style("error").red().bright(), err_count);
    } else {
        println!("{}: aborting due to previous error", style("error").red().bright());
    }
    println!();
}

fn main() {
    println!("  {} `{}`", style("Assembling").green().bright(), "filename.asm");

    let contents = get_file_contents();

    let token_stream = match TokenStream::tokenize(&contents) {
        Ok(stream) => stream,
        Err(e) => {
            display_errors(&contents, &e[..]);
            return;
        }
    };

    let statements = match Statement::translate(token_stream.as_ref()) {
        Ok(statements) => statements,
        Err(e) => {
            display_errors(&contents, &e[..]);
            return;
        }
    };

    println!("Result:\n\n{:#?}", statements);

    /*let lexer = Lexer::from_str(&contents);
    if lexer.is_err() {
        display_errors(&contents, lexer.errors());
        return;
    }

    let parser = lexer.parse().unwrap();
    if parser.is_err() {
        display_errors(&contents, parser.errors());
        return;
    }*/

    //println!("Result:\n\n{:#?}", parser);

    /*let token_stream: Vec<TokenStreamNode> = token_stream.into_iter()
        .map(Result::unwrap)
        .collect();

    println!("{:#?}", token_stream);*/


    // TODO: Preprocessor


}