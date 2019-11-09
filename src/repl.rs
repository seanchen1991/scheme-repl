use std::io;
use std::io::prelude::*;

use crate::serr::SErr;
use crate::environment::Env;
use crate::evaluator::parse_eval;

static PROMPT: &str = "scheme > ";

pub fn run(env: &mut Env) {
  loop {
    print!("{}", PROMPT);
    io::stdout().flush().expect("failed to flush to stdout");
    
    let mut expr = String::new();
    io::stdin().read_line(&mut expr).expect("failed to read from stdin");
    
    if expr.trim() == "quit" {
      break;
    }
    
    match parse_eval(expr, env) {
      Ok(result) => println!("> {}", result),
      Err(e) => match e {
        SErr::Reason(msg) => println!("> Error: {}", msg)
      }
    }
  }
}