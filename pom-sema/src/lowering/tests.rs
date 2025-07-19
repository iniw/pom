use pom_lexer::lex;
use pom_parser::parse;
use pom_utils::snap;

use crate::{
    ir::Ir,
    lowering::{Lowering, error::Errors},
};

fn test(src: &str) -> (Ir, Errors) {
    let (tokens, _) = lex(src);
    let (ast, _) = parse(src, tokens);
    Lowering::new(src).lower(ast)
}

#[test]
fn scopes() {
    snap!(test(
        r#"
a := 55;
b := a + a;
"#
    ));
    snap!(test(
        r#"
{
    a := 55;
    b := a + a;
}
a := 55;
"#
    ));
    snap!(test(
        r#"
{
    a := 55;
    b := a + a;
}
a := a;
"#
    ));
}

#[test]
fn fn_scopes() {
    snap!(test(
        r#"
a: fn = {
    b := 55;
    c := b + b;
};
"#
    ));
    snap!(test(
        r#"
a: fn (a: i32) = {
    b := a + a;
};
"#
    ));
    snap!(test(
        r#"
a: fn (b: i32) = {
    c := b + b;
};
"#
    ));
    snap!(test(
        r#"
a: fn (b: i32) = {
    b := b + b;
};
"#
    ));
}

#[test]
fn invalid_fn_scopes() {
    snap!(test(
        r#"
a: fn = {
    b := 55;
    c := b + b;
};

d := b;
e := c;
"#
    ));
    snap!(test(
        r#"
a: fn (b: i32) = {
    c := b + b;
};

d := b;
e := c;
"#
    ));
}
