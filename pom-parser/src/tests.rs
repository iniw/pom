use super::*;
use pom_lexer::lex;
use pom_utils::snap;

fn test(src: &str) -> (Ast, Errors) {
    let (tokens, _) = lex(src);
    parse(src, tokens)
}

#[test]
fn empty() {
    snap!(test(""));
    snap!(test(";"));
}

#[test]
fn precedence() {
    snap!(test("(1 + 1) / 2 * 3 + 1;"));
}

#[test]
fn bind() {
    snap!(test("var := 55;"));
    snap!(test("55 := 47;"));
    snap!(test("var: u32 = 47;"));
    snap!(test("var: 55 = 47;"));
    snap!(test("var: u32;"));
}

#[test]
fn bind_fn() {
    snap!(test("f: fn = {};"));
    snap!(test("f: fn () = {};"));
    snap!(test("f: fn (x: u32) = x;"));
    snap!(test("f: fn (a: u32 = 55) = {};"));
}

#[test]
fn invalid_bind_fn() {
    snap!(test("f: fn (a: i32) = ;"));
    snap!(test("f: fn (a := 55;"));
    snap!(test("f: fn (a = 55;"));
    snap!(test("f: fn (x) = x;"));
    snap!(test("f: fn (x = 55) = x;"));
}

#[test]
fn block_stmt() {
    snap!(test("{55 + 47; 1 * 1;}"));
    snap!(test("{55 + 47;}"));
    snap!(test("{;}"));
    snap!(test("{}"));
}

#[test]
fn invalid_block_stmt() {
    snap!(test("{55 + 47;;"));
    snap!(test("{55 + 47;"));
    snap!(test("{55 + 47"));
    snap!(test("{;"));
    snap!(test("{"));
}

#[test]
fn paren() {
    snap!(test("(55 + 47);"));
    snap!(test("(10);"));
    snap!(test("();"));
}

#[test]
fn invalid_paren() {
    snap!(test("(55 + 47;;"));
    snap!(test("(55 + 47;"));
    snap!(test("(55 + 47"));
    snap!(test("(;);"));
    snap!(test("(;"));
    snap!(test("("));
}

#[test]
fn call() {
    snap!(test("f();"));
    snap!(test("f(55);"));
    snap!(test("f(55, 47);"));
    snap!(test("f(55,);"));
    snap!(test("f(55)(47);"));
}

#[test]
fn invalid_tokens() {
    snap!(test("55 ^ $ 47"));
}
