extern crate nom;
extern crate nom_locate;

use nom::{
    IResult,
    bytes::complete::{tag, take_while, take_while1, take_until},
    character::{is_alphanumeric},
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
    let (s, _) = take_until("\n")(s)?;
    let (s, _) = tag("\n")(s)?;

    Ok((s, Token::Null))
}

fn parse_atom(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, content) = take_while1(is_alphanumeric)(s)?;

    Ok((s, Token::Atom {
        position: pos,
        content: content.fragment,
    }))
}

fn parse_sexp(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, _) = tag("(")(s)?;
    let (s, inner) = nom::multi::many_till(parse_token, tag(")"))(s)?;

    Ok((s, Token::Sexp {
        position: pos,
        inner: inner.0,
    }))
}

fn parse_token(s: Span) -> IResult<Span, Token> {
    let (s, _) = take_while(nom::character::is_space)(s)?;
    alt((
        parse_comment,
        parse_sexp,
        parse_atom
    ))(s)
}

fn main() {
    let input = Span::new("(foo) ;blubber\nbar baz".as_bytes());
    let parsed = nom::combinator::all_consuming(nom::multi::many0(parse_token))(input);
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
