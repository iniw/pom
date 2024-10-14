use std::{env, fs, path::PathBuf};

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

        let lexer = pom::lex::Lexer::new(&src);
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

        let parser = pom::syn::Parser::new(tokens);
        let (statements, errors) = parser.parse();

        if !errors.is_empty() {
            eprintln!("Parsing errors");
            for e in errors {
                dbg!(e);
            }
            eprintln!();
        }

        if !statements.is_empty() {
            eprintln!("Statements");
            for s in &statements {
                dbg!(s);
            }
            eprintln!();
        }
    }
}
