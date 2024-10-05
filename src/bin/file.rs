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

        let mut lexer = pom::lex::Lexer::new(&src);
        dbg!(lexer.lex());
    }
}
