use std::fmt;

use crate::environment::SLambda;
use crate::serr::SResult;

#[derive(Clone)]
pub enum Expression {
  Bool(bool),
  Symbol(String),
  Number(f64),
  List(Vec<Expression>),
  Func(fn(&[Expression]) -> SResult<Expression>),
  Lambda(SLambda)
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let str = match self {
      Expression::Bool(b) => {
        if *b { "#t".to_string() } else { "#f".to_string() }
      },
      Expression::Symbol(s) => s.clone(),
      Expression::Number(n) => n.to_string(),
      Expression::List(l) => {
        let chars: Vec<String> = l.iter()
          .map(|c| c.to_string())
          .collect();
        
        format!("({})", chars.join(","))
      },
      Expression::Func(_) => "Function: {}".to_string(),
      Expression::Lambda(_) => "Lambda: {}".to_string()
    };

    write!(f, "{}", str)
  }
}

