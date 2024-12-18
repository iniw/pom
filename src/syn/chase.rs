use crate::lex::{span::Spanned, Token};

macro_rules! chase {
    ($tokens:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        if let Some(token) = $tokens.next_if(|next| match **next {
                $pattern $(if $guard)? => true,
                _ => false,
            }) {
            crate::syn::chase::ChaseResult::Caught(token)
        } else {
            crate::syn::chase::ChaseResult::Missing(*$tokens.peek().expect("ICE: Shouldn't reach end of token stream when chasing."))
        }
    }};
}

pub(crate) use chase;

#[derive(Debug, Copy, Clone)]
pub enum ChaseResult<'lex> {
    Caught(Spanned<Token<'lex>>),
    Missing(Spanned<Token<'lex>>),
}
