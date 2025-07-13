use pom_lexer::token::Token;

macro_rules! chase {
    ($parser:expr, $pattern:pat $(if $guard:expr)?) => {{
        let token = $parser.tokens[$parser.cursor];
        match token.kind {
            $pattern $(if $guard)? => {
                $parser.cursor += 1;
                $crate::chase::ChaseResult::Caught(token)
            }
            _ => $crate::chase::ChaseResult::Missing(token),
        }
    }};
}

pub(crate) use chase;

#[derive(Debug, Clone, Copy)]
pub enum ChaseResult {
    Caught(Token),
    Missing(Token),
}
