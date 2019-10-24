extern crate nom;
extern crate nom_locate;

use nom::{
    IResult,
    bytes::complete::{tag, take_until, take_while1},
    character::{is_alphanumeric},
    combinator::{peek},
};
use nom_locate::{
    position,
    LocatedSpan,
};

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
enum Token<'a> {
    Atom { position: Span<'a>, content: &'a str },
    Sexp { position: Span<'a>, inner: &'a str },
}

fn alphanum(c: char) -> bool { ! is_alphanumeric(c as u8) }

fn parse_atom(s: Span) -> IResult<Span, Token> {
    let (s, content) = take_while1(alphanum)(s)?;
    let (s, pos) = position(s)?;

    Ok((s, Token::Atom {
        position: pos,
        content: content.fragment,
    }))
}

fn parse_sexp(s: Span) -> IResult<Span, Token> {
    let (s, _) = tag("(")(s)?;
    let (s, pos) = position(s)?;
    let (s, inner) = take_until(")")(s)?;
    let (s, _) = tag(")")(s)?;

    Ok((s, Token::Sexp {
        position: pos,
        inner: inner.fragment,
    }))
}

fn parse_token(s: Span) -> IResult<Span, Token> {
    let p: IResult<Span, Span> = peek(tag("("))(s);
    if p.is_ok() {
        parse_sexp(s)
    } else {
        parse_atom(s)
    }
}

fn main() {
    let input = Span::new("(foobar 3.14)");
    let output = parse_token(input).unwrap();
    match output.1 {
        Token::Sexp { position, inner, .. } => {
            println!("position: {:?}", position);
            println!("inner: {:?}", inner);
        }
        Token::Atom { position, content, .. } => {
            println!("position: {:?}", position);
            println!("content: {:?}", content);
        }
    }
}
