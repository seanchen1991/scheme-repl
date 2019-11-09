use std::fs;
use std::env;

#[macro_use]
mod parser;
mod expression;
mod serr;
mod tokenizer;
mod evaluator;
mod repl;
mod environment;

use crate::serr::SErr;
use crate::evaluator::parse_eval;
use crate::environment::init_env;

fn main() {
  let args = env::args().collect::<Vec<_>>();
  let mut env = &mut init_env();

  if args.len() == 1 {
    repl::run(&mut env);
  } else if args.len() == 2 {
    let path = &args[1];
    let input = fs::read_to_string(path).expect("Can't read file");

    match parse_eval(input, env) {
      Ok(result) => println!("> {}", result),
      Err(e) => match e {
        SErr::Reason(msg) => println!("> Error: {}", msg)
      }
    }
  }
}
