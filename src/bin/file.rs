use std::{env, fs, path::PathBuf};

use pom::{lex, syn, vm};

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
            eprintln!("Lexing errors:");
            for e in errors {
                eprintln!("  - {}", e.render(&src))
            }
            eprintln!();
        }

        if !tokens.is_empty() {
            eprintln!("Tokens:");
            for t in &tokens {
                eprintln!("  - {}", t.render(&src))
            }
            eprintln!();
        }

        let parser = syn::Parser::new(tokens);
        let (stmts, exprs, errors) = parser.parse();

        if !stmts.is_empty() {
            eprintln!("Statements:");
            for s in &stmts {
                eprintln!("  - {:?}", s)
            }
            eprintln!();
        }

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
        }

        #[rustfmt::skip]
        let program = [
            // 0x0: Call - stores pc and jumps to address 0x6
            0x0, 0x6, 0x0, 0x0, 0x0,
            // 0x7: Halt - will be executed after the function returns
            0x7,

            // "Function" - call target

            // 0x1: Reserve - function prelude, reserves 5 registers
            0x1, 0x5,
            // 0x4: LoadImm - loads 6 into register 0
            0x4, 0x0, 0x6, 0x0, 0x0, 0x0,
            // 0x6: Add - adds register 0 to register 0 and store in register 1
            0x5, 0x1, 0x0, 0x0,
            // 0x2: Ret - cleans up the reserved register and restores pc
            0x2,
        ];
        let cpu = vm::Processor::new(&program);
        cpu.run();
    }
}
