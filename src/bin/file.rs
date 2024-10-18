use std::{env, fs, path::PathBuf, time};

use pom::{lex, syn, vm};

#[allow(unused_variables)]
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

        let start = time::Instant::now();

        let lexer = lex::Lexer::new(&src);
        let (tokens, errors) = lexer.lex();

        #[cfg(debug_assertions)]
        if !errors.is_empty() {
            eprintln!("Lexing errors:");
            for e in errors {
                eprintln!("  - {}", e.render(&src))
            }
            eprintln!();
        }

        #[cfg(debug_assertions)]
        if !tokens.is_empty() {
            eprintln!("Tokens:");
            for t in &tokens {
                eprintln!("  - {}", t.render(&src))
            }
            eprintln!();
        }

        let parser = syn::Parser::new(tokens);
        let (stmts, exprs, errors) = parser.parse();

        #[cfg(debug_assertions)]
        if !stmts.is_empty() {
            eprintln!("Statements:");
            for s in &stmts {
                eprintln!("  - {:?}", s)
            }
            eprintln!();
        }

        #[cfg(debug_assertions)]
        if !exprs.is_empty() {
            eprintln!("Expressions:");
            for e in &exprs {
                eprintln!("  - {:?}", e)
            }
            eprintln!();
        }

        if !errors.is_empty() {
            eprintln!("Syntax errors:");
            for e in &errors {
                eprintln!("  - {}", e.render(&src))
            }
            eprintln!();
        } else {
            let gen = vm::Generator::new();
            let program = gen.generate(stmts, exprs);

            let cpu = vm::Processor::new(&program);
            cpu.run();

            #[cfg(debug_assertions)]
            dbg!(&program);
        }

        println!("Total execution took: {:?}", start.elapsed());
    }
}
