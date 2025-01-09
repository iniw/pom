use std::{env, fs, path::PathBuf, time};

use pom::{
    lex::Lexer,
    syn::{ParseResult, Parser},
    vm::{cpu::Processor, generator::Generator},
};

#[allow(unused_variables)]
fn main() {
    // Skip the filepath
    let args = env::args().skip(1);
    for arg in args {
        let filename = PathBuf::from(arg).with_extension("pom");

        let Ok(src) = fs::read_to_string(&filename) else {
            eprintln!("Failed to read file \"{}\".", filename.to_string_lossy());
            continue;
        };

        if src.is_empty() {
            continue;
        }

        let start = time::Instant::now();

        let lexer = Lexer::new(&src);
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

        let parser = Parser::new(&tokens);
        let ParseResult {
            global_stmts,
            stmts,
            exprs,
            errors,
        } = parser.parse();

        #[cfg(debug_assertions)]
        if !stmts.is_empty() {
            eprintln!("Outer statements:");
            for s in &global_stmts {
                let s = &stmts[*s];
                eprintln!("  - {}", s.render(&src))
            }
            eprintln!();
        }

        #[cfg(debug_assertions)]
        if !stmts.is_empty() {
            eprintln!("Statements:");
            for (_, s) in &stmts {
                eprintln!("  - {}", s.render(&src))
            }
            eprintln!();
        }

        #[cfg(debug_assertions)]
        if !exprs.is_empty() {
            eprintln!("Expressions:");
            for (_, e) in &exprs {
                eprintln!("  - {}", e.render(&src))
            }
            eprintln!();
        }

        if !errors.is_empty() {
            eprintln!("Syntax errors:");
            for e in &errors {
                eprintln!("  - {}", e.render(&src))
            }
            eprintln!();

            // Don't attempt generate/execute code if there were syntax errors.
            continue;
        }

        if stmts.is_empty() {
            continue;
        }

        let generator = Generator::new();
        match generator.generate(&global_stmts, &stmts, &exprs) {
            Ok(program) => {
                let cpu = Processor::new(&program);
                cpu.run();

                #[cfg(debug_assertions)]
                dbg!(&program);
            }
            Err(err) => eprintln!("{}", err.render(&src)),
        }

        println!("Total execution took: {:?}", start.elapsed());
    }
}
