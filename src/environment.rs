use std::rc::Rc;
use std::collections::HashMap;

use crate::serr::{SErr, SResult};
use crate::expression::Expression;
use crate::evaluator::eval_forms;
use crate::parser::{
  parse_list_of_floats,
  parse_list_of_symbols
};

pub struct Env<'a> {
  pub builtins: HashMap<String, Expression>,
  pub operations: HashMap<String, Expression>,
  pub parent_scope: Option<&'a Env<'a>>
}

#[derive(Clone)]
pub struct SLambda {
  pub params: Rc<Expression>,
  pub body: Rc<Expression>
}

pub fn init_env<'a>() -> Env<'a> {
  let operations: HashMap<String, Expression> = HashMap::new();
  let mut builtins: HashMap<String, Expression> = HashMap::new();
  
  builtins.insert(
    "+".to_string(),
    Expression::Func(|args: &[Expression]| -> SResult<Expression> {
      let sum = parse_list_of_floats(args)?.iter().sum();
      Ok(Expression::Number(sum))
    })
  );
  
  builtins.insert(
    "-".to_string(),
    Expression::Func(|args: &[Expression]| -> SResult<Expression> {
      let floats = parse_list_of_floats(args)?;
      let (first, rest) = floats.split_first().ok_or(
        SErr::Reason("expected at least one number".to_string())
      )?;
      let sum_of_rest: f64 = rest.iter().sum(); 
      Ok(Expression::Number(first - sum_of_rest))
    })
  );

  builtins.insert(
    "*".to_string(),
    Expression::Func(|args: &[Expression]| -> SResult<Expression> {
      let product = parse_list_of_floats(args)?.iter().product();
      Ok(Expression::Number(product))
    })
  );

  builtins.insert(
    "/".to_string(),
    Expression::Func(|args: &[Expression]| -> SResult<Expression> {
      let floats = parse_list_of_floats(args)?;
      let first = *floats.first().ok_or(
        SErr::Reason("expected at least one number".to_string())
      )?;
      let result = floats[1..].iter()
        .filter(|x| **x != 0.0)
        .fold(first, |num, div| num / div);
      Ok(Expression::Number(result))
    })
  );

  builtins.insert(
    "max".to_string(),
    Expression::Func(|args: &[Expression]| -> SResult<Expression> {
      let floats = parse_list_of_floats(args)?;
      let first = *floats.first().ok_or(
        SErr::Reason("`max` operation expects at least one number".to_string())
      )?;
      let max = floats.iter().fold(first, |acc, curr| acc.max(*curr));

      Ok(Expression::Number(max))
    })
  );

  builtins.insert(
    "=".to_string(),
    Expression::Func(comparison!(|a, b| a == b))
  );

  builtins.insert(
    ">".to_string(),
    Expression::Func(comparison!(|a, b| a > b))
  );

  builtins.insert(
    "<".to_string(),
    Expression::Func(comparison!(|a, b| a < b))
  );

  builtins.insert(
    ">=".to_string(),
    Expression::Func(comparison!(|a, b| a >= b))
  );

  builtins.insert(
    "<=".to_string(),
    Expression::Func(comparison!(|a, b| a <= b))
  );

  Env { builtins, operations, parent_scope: None }
}

pub fn init_lambda_env<'a>(
  params: Rc<Expression>,
  args: &[Expression],
  outer: &'a mut Env,
) -> SResult<Env<'a>> {
  let symbols = parse_list_of_symbols(params)?;

  if symbols.len() != args.len() {
    return Err(SErr::Reason(
      format!("expected {} args, got {}", symbols.len(), args.len())
    ));
  }

  let evaluated_forms = eval_forms(args, outer)?;
  let mut operations: HashMap<String, Expression> = HashMap::new();

  for (k, v) in symbols.iter().zip(evaluated_forms.iter()) {
    operations.insert(k.clone(), v.clone());
  }

  Ok(
    Env {
      operations,
      builtins: outer.builtins.clone(),
      parent_scope: Some(outer)
    }
  )
}

pub fn env_get(s: &str, env: &Env) -> Option<Expression> {
  match env.builtins.get(s) {
    Some(expr) => Some(expr.clone()),
    None => {
      match env.operations.get(s) {
        Some(expr) => Some(expr.clone()),
        None => {
          match &env.parent_scope {
            Some(outer) => env_get(s, &outer),
            None => None
          }
        }
      }
    }
  }
}