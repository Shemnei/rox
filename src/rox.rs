use std::cell::{RefCell, RefMut};
use std::io::{BufRead, Write};
use std::path::Path;

use crate::env::Environment;
use crate::interpret::Interpreter;
use crate::lex::Lexer;
use crate::parse::Parser;
use crate::source::Source;
use crate::span::Span;
use crate::symbol::{Interner, Symbol};

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug)]
pub struct Session {
    pub(crate) src: RefCell<Source>,
    pub(crate) int: RefCell<Interner>,
    pub(crate) env: RefCell<Environment>,
}

impl Session {
    pub fn new(source: Source, interner: Interner, environment: Environment) -> Self {
        Self {
            src: RefCell::new(source),
            int: RefCell::new(interner),
            env: RefCell::new(environment),
        }
    }

    pub fn src_mut(&self) -> RefMut<'_, Source> {
        self.src.borrow_mut()
    }

    pub fn env_mut(&self) -> RefMut<'_, Environment> {
        self.env.borrow_mut()
    }

    pub fn intern(&mut self, span: Span) -> Option<Symbol> {
        let src = self.src.borrow();
        Some(self.int.borrow_mut().intern(src.resolve(span)?))
    }

    pub fn get(&mut self, symbol: Symbol) -> &'static str {
        self.int.borrow().get(symbol)
    }
}

impl Default for Session {
    fn default() -> Self {
        Self {
            src: RefCell::new(Source::new()),
            int: RefCell::new(Interner::with_keywords()),
            env: RefCell::new(Environment::new()),
        }
    }
}

#[derive(Debug, Default)]
pub struct Rox {
    sess: Session,
}

impl Rox {
    pub fn new() -> Self {
        Self {
            sess: Session::default(),
        }
    }

    pub fn run_repl(&mut self) -> Result<()> {
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
            if let Err(err) = self.run(buffer.clone()) {
                eprintln!("{:?}", err);
            }

            buffer.clear();
        }
    }

    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        let bytes = std::fs::read(path)?;
        self.run(String::from_utf8(bytes)?)
    }

    fn run(&mut self, source: String) -> Result<()> {
        let scanner = Lexer::new(&source);
        let tokens = scanner.collect::<Vec<_>>();

        for token in tokens.iter() {
            eprintln!("{:?}: |{}|", token, &source.as_str()[token.span]);
        }

        //let mut parser = Parser::new(&tokens);

        //if let Some(expr) = parser.parse() {
        //    eprintln!("{:#?}", expr);
        //    let mut out = String::new();
        //    pretty_fmt(&mut out, &expr, source);
        //    eprintln!("{}", out);
        //} else {
        //    println!("No expression");
        //}

        self.sess.src_mut().add(source);

        let parser = Parser::new(&mut self.sess, &tokens);
        let stmt = parser.collect::<Vec<_>>();
        let mut interpreter = Interpreter::new(&mut self.sess);

        if let Err(err) = interpreter.interpret(stmt.into_iter()) {
            eprintln!("{}", err);
            //std::process::exit(70);
        }

        //for token in scanner {
        //    println!("{:?}: |{}|", token, &source[token.span]);
        //}

        Ok(())
    }
}
