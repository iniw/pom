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
    snap!(test("{1 + 1} / 2 * 3 + 1;"));
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
fn block() {
    snap!(test("{55 + 47; 1 * 1;};"));
    snap!(test("{55 + 47;};"));
    snap!(test("{};"));
}

#[test]
fn invalid_block() {
    snap!(test("{55 + 47;;"));
    snap!(test("{55 + 47;"));
    snap!(test("{55 + 47"));
    snap!(test("{;}"));
    snap!(test("{;"));
    snap!(test("{"));
    snap!(test("{; 1 + 1;}"));
}

#[test]
fn tuple() {
    snap!(test("(1, 2, 3);"));
    snap!(test("(1,);"));
    snap!(test("(1);"));
    snap!(test("(foo: i32, bar: f32);"));
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
    snap!(test("55 ^ 47"));
}

#[test]
fn fn_expr() {
    snap!(test("@() {};"));
    snap!(test("@(x: i32) x;"));
    snap!(test("@(x: i32) -> i32 x;"));
    snap!(test("$(x) x;"));
}

#[test]
fn bare_params() {
    snap!(test("@(x) x;"));
    snap!(test("@(x, y) x;"));
}

#[test]
fn arrow_types() {
    snap!(test("foo: (i32) -> i32 = @(x) x;"));
    snap!(test("(i32) -> (i32) -> i32;"));
    // Arrow is right-associative: a -> b -> c parses as a -> (b -> c)
    snap!(test("(i32) -> (i32) -> (i32) -> i32;"));
    // Function returning a function
    snap!(test("@(x: i32) -> (i32) -> i32 @(y: i32) x;"));
}

#[test]
fn block_grouping() {
    snap!(test("{1 + 1} / 2 * 3 + 1;"));
}

#[test]
fn tail_expr() {
    snap!(test("{1 + 1}"));
    snap!(test("{a := 5; a + 1}"));
}
