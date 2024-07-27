use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

mod callable;
mod class;
mod environment;
mod error;
mod expr;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod statement;
mod token;

use interpreter::Interpreter;
use resolver::Resolver;

use self::parser::Parser;
use self::scanner::Scanner;

use clap::{arg, command, value_parser};

fn main() {
    let matches = command!()
        .arg(arg!(
            -v --verbose ... "Turn verbose information on"
        ))
        .arg(
            arg!(
                --source <FILE> "Use some external source code file"
            )
            .required(false)
            .value_parser(value_parser!(PathBuf)),
        )
        .get_matches();

    let verbose = matches.get_one::<u8>("verbose").is_some_and(|&d| d > 0);
    let source = matches.get_one::<PathBuf>("source");

    let mut interpreter = Interpreter::new();

    if let Some(source) = source {
        let contents = fs::read_to_string(source);

        let mut scanner = Scanner::new(&contents.unwrap());
        let res = scanner.scan_tokens();
        if res.is_err() {
            println!("error while scanning tokens: {:?}", res);
            return;
        }

        let mut parser = Parser::new(scanner.tokens);
        let res = parser.parse();
        if res.is_err() {
            println!("error while parsing: {:?}", res.err());
            return;
        }

        let mut resolver = Resolver::new(&mut interpreter);
        let statements = res.unwrap();
        // for statement in &statements {
        //     println!("{:?}",statement);
        // }

        let res = resolver.resolve(&statements);
        if res.is_err() {
            println!("failed resolving: {:?}", res);
            return;
        }

        let res = interpreter.interpret(&statements);
        if res.is_err() {
            println!("failed evaluating: {:?}", res);
        }

        return;
    }

    loop {
        if verbose {
            println!("{:?}", interpreter);
        }

        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        input = input.trim().to_string();

        if input.is_empty() {
            break;
        }

        let mut scanner = Scanner::new(&input);
        let res = scanner.scan_tokens();
        if res.is_err() {
            println!("error while scanning tokens: {:?}", res);
            continue;
        }
        if verbose {
            println!("Tokens: {:?}", scanner.tokens);
        }

        let mut parser = Parser::new(scanner.tokens);
        let res = parser.parse();
        if res.is_err() {
            println!("error while parsing: {:?}", res.err());
            continue;
        }
        if verbose {
            println!("Parser: {:?}", parser);
        }

        let result = interpreter.interpret(&res.unwrap());
        if result.is_err() {
            println!("failed evaluating '{}': {:?}", input, result);
            continue;
        }

        println!("{}", result.unwrap());
        println!();
    }
}
