use std::rc::Rc;

use crate::serr::{SErr, SResult};
use crate::expression::Expression;

pub fn parse<'a>(tokens: &'a [String]) -> SResult<(Expression, &'a [String])> {
  let (first, rest) = tokens.split_first().ok_or(
    SErr::Reason("Could not get Expression".to_string())
  )?;
  
  match &first[..] {
    "(" => read_sequence(rest),
    ")" => Err(SErr::Reason("Unexpected `)`".to_string())),
    _   => Ok((parse_atom(first), rest)),
  }
}

fn read_sequence<'a>(tokens: &'a [String]) -> SResult<(Expression, &'a [String])> {
  let mut result: Vec<Expression> = vec![];
  let mut ts = tokens;

  loop {
    let (next, rest) = ts.split_first().ok_or(
      SErr::Reason("Could not find closing `)`".to_string())
    )?;
    
    if next == ")" {
      return Ok((Expression::List(result), rest));
    }

    let (exp, new_ts) = parse(&ts)?;
    result.push(exp);
    ts = new_ts;
  }
}

fn parse_atom(token: &str) -> Expression {
  match token.as_ref() {
    "#t" => Expression::Bool(true),
    "#f" => Expression::Bool(false),
    _    => {
      match token.parse() {
        Ok(n)  => Expression::Number(n),
        Err(_) => Expression::Symbol(token.to_string().clone())
      }
    }
  } 
}

pub fn parse_list_of_floats(args: &[Expression]) -> SResult<Vec<f64>> {
  args.iter()
    .map(|x| parse_float(x))
    .collect()
}

fn parse_float(exp: &Expression) -> SResult<f64> {
  match exp {
    Expression::Number(n) => Ok(*n),
    _ => Err(SErr::Reason("Expected a number".to_string()))
  }
}

pub fn parse_list_of_symbols(args: Rc<Expression>) -> SResult<Vec<String>> {
  let list = match args.as_ref() {
    Expression::List(l) => Ok(l.clone()),
    _ => Err(SErr::Reason(
      "Expected args form to be a list".to_string()
    ))
  }?;
  list.iter()
    .map(|x| {
      match x {
        Expression::Symbol(s) => Ok(s.clone()),
        _ => Err(SErr::Reason(
          "Expected symbols in the argument list".to_string()
        ))
      }
    })
    .collect()
}

#[macro_export]
macro_rules! comparison {
  ($check_fn:expr) => {{
    |args: &[Expression]| -> SResult<Expression> {
      let floats = parse_list_of_floats(args)?;
      let (first, rest) = floats.split_first().ok_or(
        SErr::Reason("Expected at least one number".to_string())
      )?;

      fn f(prev: &f64, ts: &[f64]) -> bool {
        match ts.first() {
          Some(t) => $check_fn(prev, t) && f(t, &ts[1..]),
          None => true
        }
      };

      Ok(Expression::Bool(f(first, rest)))
    }
  }}
}