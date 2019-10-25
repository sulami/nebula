extern crate nom;
extern crate nom_locate;

use nom::{
    IResult,
    bytes::complete::{tag},
    character::complete::{alphanumeric1,
                          line_ending,
                          not_line_ending,
                          multispace0},
    combinator::{all_consuming},
    multi::{many0, many_till},
    branch::alt,
};
use nom_locate::{
    position,
    LocatedSpan,
};

type Span<'a> = LocatedSpan<&'a [u8]>;

#[derive(Debug)]
enum Token<'a> {
    Atom { position: Span<'a>, content: &'a [u8] },
    Sexp { position: Span<'a>, inner: Vec<Token<'a>> },
    Null,
}

fn parse_comment(s: Span) -> IResult<Span, Token> {
    let (s, _) = tag(";")(s)?;
    let (s, _) = not_line_ending(s)?;
    let (s, _) = line_ending(s)?;

    Ok((s, Token::Null))
}

fn parse_atom(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, content) = alphanumeric1(s)?;

    Ok((s, Token::Atom {
        position: pos,
        content: content.fragment,
    }))
}

fn parse_sexp(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, _) = tag("(")(s)?;
    let (s, inner) = many_till(parse_token, tag(")"))(s)?;

    Ok((s, Token::Sexp {
        position: pos,
        inner: inner.0,
    }))
}

fn parse_token(s: Span) -> IResult<Span, Token> {
    let (s, _) = multispace0(s)?;
    alt((
        parse_comment,
        parse_sexp,
        parse_atom
    ))(s)
}

fn parse_source(source: &str) {
    println!("Parsing:\n\n{}\n", source);
    let input = Span::new(source.as_bytes());
    let parsed = all_consuming(many0(parse_token))(input);
    match parsed {
        Ok((_, tokens)) => for token in tokens {
            match token {
                Token::Sexp { position, inner, .. } => {
                    println!("position: {:?}", position);
                    println!("inner: {:?}", inner);
                }
                Token::Atom { position, content, .. } => {
                    println!("position: {:?}", position);
                    println!("content: {:?}", content);
                }
                Token::Null => println!("Null")
            }
        }
        Err(_) => println!("Failed parsing")
    }
}

fn main() {
    parse_source("(foo)\n ; blubber\nbar baz");
}
