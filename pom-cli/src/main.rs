use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};
use pom_lexer::lex;
use pom_parser::parse;
use pom_sema::analyse;

fn main() {
    let cli = Cli::parse();
    let (src, debug) = match cli.cmd {
        Command::Run { file, debug } => {
            let Ok(src) = fs::read_to_string(file.with_extension("pom")) else {
                eprintln!(
                    "File '{}' doesn't exist or could not be read",
                    file.to_string_lossy()
                );
                return;
            };
            (src, debug)
        }
        Command::Eval { expr, debug } => (expr, debug),
    };

    run(&src, debug);
}

fn run(src: &str, debug: Debug) {
    let (tokens, errors) = lex(src);

    if debug.should_debug(Debug::Lexer) {
        println!("[LEXER] Tokens: {tokens:#?}");
        println!("[LEXER] Errors: {errors:#?}");
    }

    let (ast, errors) = parse(src, tokens);

    if debug.should_debug(Debug::Parser) {
        println!("[PARSER] Ast: {ast:#?}");
        println!("[PARSER] Errors: {errors:#?}");
    }

    let (ir, lowering_errors, typecheck_errors) = analyse(src, ast);

    if debug.should_debug(Debug::Sema) {
        println!("[SEMA] Ir: {ir:#?}");
        println!("[SEMA] Lowering errors: {lowering_errors:#?}");
        println!("[SEMA] Typecheck errors: {typecheck_errors:#?}");
    }
}

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
        debug: Debug,
    },
    Eval {
        expr: String,
        #[arg(short, long)]
        debug: Debug,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
enum Debug {
    Lexer,
    Parser,
    Sema,
    All,
}

impl Debug {
    fn should_debug(self, debug: Debug) -> bool {
        self == debug || self == Debug::All
    }
}
