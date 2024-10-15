use std::{env, fs, path::PathBuf};

use pom::{eval, lex, syn};

fn main() {
    // Skip the filepath
    let args = env::args().skip(1);
    for arg in args {
        let filename = PathBuf::from(arg).with_extension("pom");

        let src = if let Ok(src) = fs::read_to_string(&filename) {
            src
        } else {
            eprintln!("Failed to read file \"{}\".", filename.to_string_lossy());
            continue;
        };

        let lexer = lex::Lexer::new(&src);
        let (tokens, errors) = lexer.lex();

        if !errors.is_empty() {
            eprintln!("Lexing errors");
            for e in errors {
                eprintln!("  - {}", e.render(&src))
            }
            eprintln!();
        }

        if !tokens.is_empty() {
            eprintln!("Tokens");
            for t in &tokens {
                eprintln!("  - {}", t.render(&src))
            }
            eprintln!();
        }

        let parser = syn::Parser::new(tokens);
        let result = parser.parse();
        dbg!(&result);

        let mut interpreter = eval::Interpreter::new();
        let result = interpreter.evaluate(result.0, result.1);
        dbg!(&result);
    }
}
