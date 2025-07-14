use super::*;
use pom_lexer::lex;
use pom_utils::snap;

fn lex_and_parse(src: &str) -> (Ast, Errors) {
    let (tokens, errors) = lex(src);
    assert!(errors.is_empty());
    parse(src, tokens)
}

#[test]
fn empty() {
    snap!(lex_and_parse(""));
    snap!(lex_and_parse(";"));
}

#[test]
fn precedence() {
    snap!(lex_and_parse("(1 + 1) / 2 * 3 + 1;"));
}

#[test]
fn bind() {
    snap!(lex_and_parse("var := 55;"));
    snap!(lex_and_parse("55 := 47;"));
    snap!(lex_and_parse("var: u32 = 47;"));
    snap!(lex_and_parse("var: 55 = 47;"));
    snap!(lex_and_parse("var: u32;"));
}

#[test]
fn block_stmt() {
    snap!(lex_and_parse("{55 + 47; 1 * 1;}"));
    snap!(lex_and_parse("{55 + 47;}"));
    snap!(lex_and_parse("{;}"));
    snap!(lex_and_parse("{}"));
}

#[test]
fn invalid_block_stmt() {
    snap!(lex_and_parse("{55 + 47;;"));
    snap!(lex_and_parse("{55 + 47;"));
    snap!(lex_and_parse("{55 + 47"));
    snap!(lex_and_parse("{;"));
    snap!(lex_and_parse("{"));
}

#[test]
fn paren() {
    snap!(lex_and_parse("(55 + 47);"));
    snap!(lex_and_parse("(10);"));
    snap!(lex_and_parse("();"));
}

#[test]
fn invalid_paren() {
    snap!(lex_and_parse("(55 + 47;;"));
    snap!(lex_and_parse("(55 + 47;"));
    snap!(lex_and_parse("(55 + 47"));
    snap!(lex_and_parse("(;);"));
    snap!(lex_and_parse("(;"));
    snap!(lex_and_parse("("));
}
