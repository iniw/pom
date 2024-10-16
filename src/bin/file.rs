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

        use vm::Op::*;
        let program = [
            Call { addr: 2 },
            Halt,
            // "Function"
            Reserve { regs: 5 },
            LoadImm { dst: 0, imm: 6 },
            Add {
                dst: 1,
                left: 0,
                right: 0,
            },
            Ret,
        ];
        let cpu = vm::Processor::new(&program);
        cpu.run();
    }
}
