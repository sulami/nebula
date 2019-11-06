extern crate nom;
extern crate nom_locate;

use std::fmt::{Debug, Display, Formatter, Result};
// use std::io::BufRead;

use nom::{
    IResult,
    bytes::complete::{is_a, tag},
    character::complete::{alphanumeric1,
                          digit0,
                          digit1,
                          line_ending,
                          not_line_ending,
                          multispace1},
    combinator::{all_consuming, opt, recognize, rest},
    multi::{many0, many_till},
    branch::alt,
    sequence::{pair, tuple},
};
use nom_locate::{
    position,
    LocatedSpan,
};

type Span<'a> = LocatedSpan<&'a [u8]>;

enum TokenKind {
    Symbol,
    Keyword,
    Integer,
    Float,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let repr = match self {
            TokenKind::Symbol => "symbol",
            TokenKind::Keyword => "keyword",
            TokenKind::Integer => "int",
            TokenKind::Float => "float",
        };
        write!(f, "{}", repr)
    }
}

enum Token<'a> {
    Atom { position: Span<'a>, kind: TokenKind, content: &'a [u8] },
    Sexp { position: Span<'a>, inner: Vec<Token<'a>> },
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Token::Sexp { inner, .. } => {
                let inner_strings: Vec<String> = inner
                    .iter()
                    .map(|x| x.to_string())
                    .collect();
                write!(f, "({})", inner_strings.join(" "))
            },
            Token::Atom { content, .. } => write!(f, "{}", std::str::from_utf8(&content).expect("foo")),
        }
    }
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Token::Sexp { position, inner, .. } => {
                let inner_strings: Vec<String> = inner
                    .iter()
                    .map(|x| format!("{:?}", x))
                    .collect();
                write!(f, "{}:{} Sexp\n{}\nSexp End",
                       position.line,
                       position.offset,
                       inner_strings.join("\n"))
            }
            Token::Atom { position, kind, content, .. } => {
                write!(f, "{}:{} {}<{}>",
                       position.line,
                       position.offset,
                       std::str::from_utf8(&content).expect("bar"),
                       kind
                )
            }
        }
    }
}

fn parse_comment(s: Span) -> IResult<Span, Span> {
    let (s, _) = tag(";")(s)?;
    let (s, _) = not_line_ending(s)?;
    let (s, _) = line_ending(s)?;

    Ok((s, s)) // First one's the right one, don't read from the second one.
}

fn parse_float(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, content) = recognize(tuple((
        opt(tag("-")),
        digit0,
        tag("."),
        digit1
    )))(s)?;

    Ok((s, Token::Atom {
        position: pos,
        kind: TokenKind::Float,
        content: content.fragment,
    }))
}

fn parse_int(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, content) = recognize(pair(
        opt(tag("-")),
        digit1
    ))(s)?;

    Ok((s, Token::Atom {
        position: pos,
        kind: TokenKind::Integer,
        content: content.fragment,
    }))
}

fn parse_keyword(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, content) = recognize(pair(tag(":"), alphanumeric1))(s)?;

    Ok((s, Token::Atom {
        position: pos,
        kind: TokenKind::Keyword,
        content: content.fragment,
    }))
}

const SYMBOL_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890*+!-_'?<>=";

fn parse_symbol(s: Span) -> IResult<Span, Token> {
    let (s, pos) = position(s)?;
    let (s, content) = is_a(SYMBOL_CHARS)(s)?;

    Ok((s, Token::Atom {
        position: pos,
        kind: TokenKind::Symbol,
        content: content.fragment,
    }))
}

fn parse_atom(s: Span) -> IResult<Span, Token> {
    alt((parse_keyword, parse_float, parse_int, parse_symbol))(s)
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

fn skip_stuff(s: Span) -> IResult<Span, Span> {
    let alt_parser = alt((parse_comment, multispace1));
    let (s, _) = many0(alt_parser)(s)?;
    rest(s)
}

fn parse_token(s: Span) -> IResult<Span, Token> {
    let (_, s) = skip_stuff(s)?;
    alt((
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
            println!("{:?}", token)
        }
        Err(e) => println!("Failed parsing {:?}", e)
    }
}

use inkwell::OptimizationLevel;
use inkwell::context::Context;
// use inkwell::builder::Builder;
// use inkwell::module::Module;
use inkwell::targets::{Target, TargetMachine, RelocMode, CodeModel, FileType, InitializationConfig};
use std::path::Path;

fn main() {
    parse_source("(foo 3432 (dag of dags))\n ; blubber\nbar baz :kw");

    let context = Context::create();
    let module = context.create_module("toast");
    let builder = context.create_builder();
    let main_fn_type = context.i32_type().fn_type(&[], false); // arguments & is_var_args
    let main_fn_val = module.add_function("main", main_fn_type, None);
    let entry = main_fn_val.append_basic_block("entry");
    let number = context.i32_type().const_int(4, false);
    builder.position_at_end(&entry);
    builder.build_return(Some(&number));

    // jit compile & execution
    let jee = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    unsafe {
        let rv = jee.run_function_as_main(&main_fn_val, &[]);
        println!("{}", rv);
    }

    // write llvm bitcode to a file
    // might be useful for caching I guess, if we ever have performance problems
    // let path = Path::new("toast.bc");
    // module.write_bitcode_to_path(&path);

    // write a native object file
    // can be picked up with llc/gcc
    Target::initialize_x86(&InitializationConfig::default());
    let path = Path::new("toast.o");
    let target = Target::from_name("x86-64").expect("failed to create target");
    let target_machine = target.create_target_machine(
        &TargetMachine::get_default_triple().to_string(),
        "generic",
        "",
        OptimizationLevel::None,
        RelocMode::Default,
        CodeModel::Default,
    ).expect("failed to create target machine");
    let result = target_machine.write_to_file(
        &module,
        FileType::Object,
        &path,
    );
    match result {
        Ok(_) => {},
        Err(_) => println!("failed to write"),
    }

    // for line in std::io::stdin().lock().lines() {
    //     let l = line.unwrap();
    //     if l.is_empty() {
    //         std::process::exit(0);
    //     } else {
    //         parse_source(&String::from(l));
    //     }
    // }
}
