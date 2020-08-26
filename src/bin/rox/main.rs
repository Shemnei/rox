use std::ffi::OsString;
use std::path::Path;

use rox::{expr::pretty_fmt, interpret::Interpreter, lex::Lexer, parse::Parser};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

static INTERPRETER: Interpreter = Interpreter::new();

fn main() -> Result<()> {
    let args = std::env::args_os().skip(1).collect::<Vec<OsString>>();
    match args.as_slice() {
        [] => run_prompt(),
        [path] => run_file(path),
        [..] => {
            eprintln!("Usage: {} [script]", env!("CARGO_PKG_NAME"));
            std::process::exit(64);
        }
    }
}

fn run_file<P: AsRef<Path>>(path: P) -> Result<()> {
    let bytes = std::fs::read(path)?;
    run(&String::from_utf8(bytes)?)
}

fn run_prompt() -> Result<()> {
    use std::io::{BufRead, Write};

    let input = std::io::stdin();
    let mut input = input.lock();
    let mut output = std::io::stdout();

    let mut buffer = String::new();

    loop {
        write!(&mut output, "> ")?;
        output.flush()?;

        if input.read_line(&mut buffer)? == 0 {
            break Ok(());
        }
        if let Err(err) = run(&buffer) {
            eprintln!("{:?}", err);
        }
        buffer.clear();
    }
}

fn run(source: &str) -> Result<()> {
    let scanner = Lexer::new(source);
    let tokens = scanner.collect::<Vec<_>>();

    for token in tokens.iter() {
        eprintln!("{:?}: |{}|", token, &source[token.span]);
    }

    let mut parser = Parser::new(&tokens);

    if let Some(expr) = parser.parse() {
        eprintln!("{:#?}", expr);
        let mut out = String::new();
        pretty_fmt(&mut out, &expr, source);
        eprintln!("{}", out);
        if let Err(err) = INTERPRETER.interpret(source, &expr) {
            eprintln!("{}", err);
            //std::process::exit(70);
        }
    } else {
        println!("No expression");
    }

    //for token in scanner {
    //    println!("{:?}: |{}|", token, &source[token.span]);
    //}

    Ok(())
}
