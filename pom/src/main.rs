use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};
use pom_lexer::lex;
use pom_parser::parse;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    cmd: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Run {
        file: PathBuf,
        #[arg(short, long)]
        debug: bool,
    },
    Eval {
        expr: String,
        #[arg(short, long)]
        debug: bool,
    },
}

fn run(src: &str, debug: bool) {
    let (tokens, errors) = lex(src);

    if debug {
        println!("[LEXER] Tokens: {:#?}", tokens);
        println!("[LEXER] Errors: {:#?}", errors);
    }

    let (ast, errors) = parse(src, tokens);

    if debug {
        println!("[PARSER] Ast: {:#?}", ast);
        println!("[PARSER] Errors: {:#?}", errors);
    }
}

fn main() {
    let cli = Cli::parse();
    match cli.cmd {
        Command::Run { file, debug } => {
            let Ok(src) = fs::read_to_string(file.with_extension("pom")) else {
                eprintln!(
                    "File '{}' doesn't exist or could not be read",
                    file.to_string_lossy()
                );
                return;
            };
            run(&src, debug);
        }
        Command::Eval { expr, debug } => {
            run(&expr, debug);
        }
    }
}
