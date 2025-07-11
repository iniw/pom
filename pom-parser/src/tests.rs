use super::*;
use pom_lexer::lex;
use pom_utils::snap;

fn lex_and_parse(src: &str) -> (Ast, Errors) {
    let (tokens, errors) = lex(src);
    assert!(errors.is_empty());
    parse(src, tokens)
}

#[test]
fn sums() {
    snap!(lex_and_parse("1 + 1 + 1"));
}
