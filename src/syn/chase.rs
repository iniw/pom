use crate::lex::{Token, span::Spanned};

macro_rules! chase {
    ($parser:expr, $pattern:pat $(if $guard:expr)?) => {{
        let token = $parser.tokens.get($parser.position).expect("ICE: Reached end of token stream when chasing.");
        if matches!(**token, $pattern $(if $guard)?) {
            $parser.position += 1;
            crate::syn::chase::ChaseResult::Caught(**token)
        } else {
            crate::syn::chase::ChaseResult::Missing(*token)
        }
    }};

    ($parser:expr, $($pattern:pat $(if $guard:expr)?),+) => {{
        const N: usize = ${count($pattern)};

        let mut results = [std::mem::MaybeUninit::uninit(); N];
        let mut matched_all = true;
        $(
            let token = $parser.tokens.get($parser.position + ${index()}).expect("ICE: Reached end of token stream when chasing.");
            if matches!(**token, $pattern $(if $guard)?) {
                results[${index()}].write($crate::syn::chase::ChaseResult::Caught(**token));
            } else {
                results[${index()}].write($crate::syn::chase::ChaseResult::Missing(*token));
                matched_all = false;
            }
        )+

        if matched_all {
            $parser.position += N;
        }

        unsafe {
            std::mem::transmute::<
                [std::mem::MaybeUninit<$crate::syn::chase::ChaseResult<_>>; N],
                [$crate::syn::chase::ChaseResult<_>; N]>(results)
        }
    }};
}

macro_rules! spanned_chase {
    ($parser:expr, $pattern:pat $(if $guard:expr)?) => {{
        let token = $parser.tokens.get($parser.position).expect("ICE: Reached end of token stream when chasing.");
        if matches!(**token, $pattern $(if $guard)?) {
            $parser.position += 1;
            crate::syn::chase::ChaseResult::Caught(*token)
        } else {
            crate::syn::chase::ChaseResult::Missing(*token)
        }
    }};

    ($parser:expr, $($pattern:pat $(if $guard:expr)?),+) => {{
        const N: usize = ${count($pattern)};

        let mut results = [std::mem::MaybeUninit::uninit(); N];
        let mut matched_all = true;
        $(
            let token = $parser.tokens.get($parser.position + ${index()}).expect("ICE: Reached end of token stream when chasing.");
            if matches!(**token, $pattern $(if $guard)?) {
                results[${index()}].write($crate::syn::chase::ChaseResult::Caught(*token));
            } else {
                results[${index()}].write($crate::syn::chase::ChaseResult::Missing(*token));
                matched_all = false;
            }
        )+

        if matched_all {
            $parser.position += N;
        }

        unsafe {
            std::mem::transmute::<
                [std::mem::MaybeUninit<$crate::syn::chase::ChaseResult<_>>; N],
                [$crate::syn::chase::ChaseResult<_>; N]>(results)
        }
    }};
}

pub(crate) use chase;
pub(crate) use spanned_chase;

#[derive(Debug, Copy, Clone)]
pub enum ChaseResult<'lex, T> {
    Caught(T),
    Missing(Spanned<Token<'lex>>),
}
