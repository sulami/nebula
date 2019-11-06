use std::fmt::{Debug, Display, Formatter, Result};
use std::io::BufRead;

// ----------------------------------------------------------------------------
// Parsing
// ----------------------------------------------------------------------------

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

fn parse_source(source: &str) -> Vec<Token> {
    // println!("Parsing:\n\n{}\n", source);
    let input = Span::new(source.as_bytes());
    let tokens: IResult<Span, Vec<Token>> = all_consuming(many0(parse_token))(input);
    match tokens {
        Ok((_, ts)) => ts,
        Err(_) => vec!(),
    }
}

// ----------------------------------------------------------------------------
// Compiler
// ----------------------------------------------------------------------------

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
// use inkwell::targets::{Target, TargetMachine, RelocMode, CodeModel, FileType, InitializationConfig};
use inkwell::execution_engine::ExecutionEngine;
use inkwell::values::{BasicValueEnum, FunctionValue, GenericValue};
use inkwell::types::BasicTypeEnum;
// use std::path::Path;

struct Compiler {
    context: Context,
    module: Module,
    builder: Builder,
    jit: ExecutionEngine,
}

fn init_compiler() -> Compiler {
    let ctx = Context::create();
    let module = ctx.create_module("nebula");
    let builder = ctx.create_builder();
    let jit = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    Compiler {
        context: ctx,
        module: module,
        builder: builder,
        jit: jit,
    }
}

impl Compiler {
    // TODO needs to add a new module on every interaction
    // might need to carry over previous scope into the new module as well
    // see this loop:
    // https://github.com/TheDan64/inkwell/blob/master/examples/kaleidoscope/main.rs#L1244

    fn add_function(&self, name: &str, args: &[BasicTypeEnum], is_var_args: bool) -> FunctionValue {
        // TODO variable return type
        let fn_type = self.context.i32_type().fn_type(args, is_var_args);
        self.module.add_function(name, fn_type, None)
    }

    fn add_constant(&self, value: u64) -> BasicValueEnum {
        //TODO variable type
        BasicValueEnum::IntValue(self.context.i32_type().const_int(value, false))
    }

    fn add_return(&self, value: BasicValueEnum) {
        match value {
            BasicValueEnum::IntValue(v) => self.builder.build_return(Some(&v)),
            _ => panic!("only ints"),
        };
    }

    fn call_function(&self, name: &str, _args: &[&GenericValue]) -> i32 {
        // TODO arguments
        // TODO variable types
        // TODO handle lookup errors
        let maybe_fn = unsafe {
            self.jit.get_function::<unsafe extern "C" fn() -> i32>(name)
        };
        let fun = match maybe_fn {
            Ok(f) => f,
            Err(_) => panic!("no function"),
        };
        unsafe {
            fun.call()
        }
    }
}

// ----------------------------------------------------------------------------
// REPL
// ----------------------------------------------------------------------------

use std::io::Write;

macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

fn main() {
    // parse_source("(foo 3432 (dag of dags))\n ; blubber\nbar baz :kw");
    let compiler = init_compiler();

    // write llvm bitcode to a file
    // might be useful for caching I guess, if we ever have performance problems
    // let path = Path::new("toast.bc");
    // module.write_bitcode_to_path(&path);

    // write a native object file
    // can be picked up with llc/gcc
    // Target::initialize_x86(&InitializationConfig::default());
    // let path = Path::new("toast.o");
    // let target = Target::from_name("x86-64").expect("failed to create target");
    // let target_machine = target.create_target_machine(
    //     &TargetMachine::get_default_triple().to_string(),
    //     "generic",
    //     "",
    //     OptimizationLevel::None,
    //     RelocMode::Default,
    //     CodeModel::Default,
    // ).expect("failed to create target machine");
    // let result = target_machine.write_to_file(
    //     &compiler.module,
    //     FileType::Object,
    //     &path,
    // );
    // match result {
    //     Ok(_) => {},
    //     Err(_) => println!("failed to write"),
    // }

    // TODO convert to loop
    print_flush!("> ");
    for line in std::io::stdin().lock().lines() {
        let l = line.unwrap();
        if l.is_empty() {
            std::process::exit(0);
        } else {
            for token in parse_source(&String::from(l)) {
                println!("parsed: {:#?}", token);
                match token {
                    Token::Atom{kind: TokenKind::Integer, content, ..} => {
                        let fn_val = compiler.add_function("repl", &[], false);
                        let entry = fn_val.append_basic_block("entry");
                        let n = compiler.add_constant(
                            std::str::from_utf8(content)
                                .unwrap()
                                .parse::<u64>()
                                .unwrap());
                        // XXX why do we need this?
                        compiler.builder.position_at_end(&entry);
                        compiler.add_return(n);
                        println!("{:?}", compiler.call_function("repl", &[]));
                    },
                    _ => panic!("don't know what to do")
                }
            }
            print_flush!("> ");
        }
    }
}
