use std::rc::Rc;

use crate::serr::{SErr, SResult};
use crate::expression::Expression;
use crate::parser::parse;
use crate::environment::{
  Env,
  SLambda,
  env_get,
  init_lambda_env,
};

pub fn eval(exp: &Expression, env: &mut Env) -> SResult<Expression> {
  match exp {
    Expression::Bool(_)   => Ok(exp.clone()),
    Expression::Symbol(s) => env_get(s, env).ok_or(
      SErr::Reason(format!("unexpected symbol: '{}'", s))
    ),
    Expression::Number(_) => Ok(exp.clone()),
    Expression::List(l) => {
      let (first, args) = l.split_first().ok_or(
        SErr::Reason("expected a non-empty list".to_string())
      )?;
      match eval_keyword(first, args, env) {
        Some(result) => result,
        None => {
          let first_eval = eval(first, env)?;
          match first_eval {
            Expression::Func(f) => {
              f(&eval_forms(args, env)?)
            },
            Expression::Lambda(l) => {
              let new_env = &mut init_lambda_env(l.params, args, env)?;
              eval(&l.body, new_env)
            },
            _ => Err(SErr::Reason("first form must be a function".to_string()))
          }
        }
      }
    },
    Expression::Func(_) => Err(SErr::Reason("unexpected form".to_string())),
    Expression::Lambda(_) => Err(SErr::Reason("unexpected form".to_string()))
  }
}

pub fn eval_forms(args: &[Expression], env: &mut Env) -> SResult<Vec<Expression>> {
  args
    .iter()
    .map(|x| eval(x, env))
    .collect()
}

fn eval_keyword(
  expr: &Expression, 
  args: &[Expression], 
  env: &mut Env
) -> Option<SResult<Expression>> {
  match expr {
    Expression::Symbol(s) => {
      match s.as_ref() {
        "if"     => Some(eval_if(args, env)),
        "define" => Some(eval_define(args, env)),
        "set!"   => Some(eval_set(args, env)),
        "lambda" => Some(eval_lambda(args)),
        _ => None,
      }
    },
    _ => None
  }
}

fn eval_if(args: &[Expression], env: &mut Env) -> SResult<Expression> {
  let criteria = args.first().ok_or(SErr::Reason("expected criteria".to_string()))?;
  let criteria_eval = eval(criteria, env)?;

  match criteria_eval {
    Expression::Bool(b) => {
      let branch = if b { 1 } else { 2 };
      let result = args.get(branch).ok_or(
        SErr::Reason(format!("expected branching conditional: {}", branch))
      )?;
      eval(result, env)
    },
    _ => Err(
      SErr::Reason(format!("unexpected criteria: '{}'", criteria.to_string()))
    )
  }
}

fn eval_define(args: &[Expression], env: &mut Env) -> SResult<Expression> {
  if args.len() > 2 {
    return Err(SErr::Reason("`define` keyword only accepts two forms".to_string()));
  }

  let (name, rest) = args.split_first().ok_or(
    SErr::Reason("expected first form".to_string())
  )?;
  let name_str = match name {
    Expression::Symbol(s) => Ok(s.clone()),
    _ => Err(SErr::Reason("expected first form to be a symbol".to_string()))
  }?;

  if env.operations.contains_key(&name_str) {
    return Err(SErr::Reason("can not overwrite a reserved operation".to_string()));
  }  

  let value = rest.get(0).ok_or(
    SErr::Reason("expected a value form".to_string())
  )?;
  let value_eval = eval(value, env)?;

  env.operations.insert(name_str, value_eval);
  Ok(name.clone())
}

fn eval_set(args: &[Expression], env: &mut Env) -> SResult<Expression> {
  if args.len() > 2 {
    return Err(SErr::Reason("`define` keyword only accepts two forms".to_string()));
  }

  let (name, rest) = args.split_first().ok_or(
    SErr::Reason("expected first form".to_string())
  )?;
  let name_str = match name {
    Expression::Symbol(s) => Ok(s.clone()),
    _ => Err(SErr::Reason("expected name to be a symbol".to_string()))
  }?;

  let value = rest.get(0).ok_or(
    SErr::Reason("expected a value form".to_string())
  )?;
  let value_eval = eval(value, env)?;

  match env.operations.get(&name_str) {
    Some(_) => {
      env.operations.insert(name_str, value_eval);
    },
    None => return Err(SErr::Reason(format!("global variable '{}' is not defined", name_str)))
  }

  Ok(name.clone())
}

fn eval_lambda(args: &[Expression]) -> SResult<Expression> {
  if args.len() > 2 {
    return Err(SErr::Reason("lambda requires exactly two forms".to_string()));
  }

  let params = args.first().ok_or(SErr::Reason("expected lambda param".to_string()))?;
  let body = args.get(1).ok_or(SErr::Reason("expected lambda body".to_string()))?;

  Ok(Expression::Lambda(
    SLambda {
      params: Rc::new(params.clone()),
      body: Rc::new(body.clone())
    }
  ))
}

pub fn parse_eval(input: Vec<String>, env: &mut Env) -> SResult<Expression> {
  let (parsed, unparsed) = parse(&input)?;
  let evaluated = eval(&parsed, env)?;

  if !unparsed.is_empty() {
    return parse_eval(unparsed.to_vec(), env);
  } else {
    Ok(evaluated)
  }
}