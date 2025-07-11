use super::*;
use pom_utils::snap;

#[test]
fn whitespace() {
    snap!(lex(" \t\r\n"));
}

#[test]
fn booleans() {
    snap!(lex("true false"));
}

#[test]
fn idents() {
    snap!(lex("w w1 _ _w1 W W1 W1_1W"));
}

#[test]
fn maximal_munch() {
    snap!(lex("truee falsee"));
}

#[test]
fn numbers() {
    snap!(lex("55 55.47 -55 +47.55"));
}

#[test]
fn operators() {
    snap!(lex("+ -"));
}

#[test]
fn errors() {
    snap!(lex("55 ^ $ 47"));
}
