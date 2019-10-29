use std::io;
use std::fmt;
use std::rc::Rc;
use std::io::prelude::*;
use std::collections::HashMap;

static PROMPT: &str = "scheme > ";

#[derive(Debug)]
enum SErr {
  Reason(String)
}

impl fmt::Display for SErr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let str = match self {
      SErr::Reason(r) => r.clone()
    };

    write!(f, "{}", str)
  }
}

type SResult<T> = Result<T, SErr>;

#[derive(Clone)]
enum Expression {
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

struct Env<'a> {
  operations: HashMap<String, Expression>,
  scope: Option<&'a Env<'a>>
}

#[derive(Clone)]
struct SLambda {
  params: Rc<Expression>,
  body: Rc<Expression>
}

fn tokenize(expression: String) -> Vec<String> {
  expression
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split_whitespace()
    .map(|x| x.to_string())
    .collect()
}

fn parse<'a>(tokens: &'a [String]) -> SResult<(Expression, &'a [String])> {
  let (first, rest) = tokens.split_first().ok_or(
    SErr::Reason("could not get Expression".to_string())
  )?;
  
  match &first[..] {
    "(" => read_sequence(rest),
    ")" => Err(SErr::Reason("unexpected `)`".to_string())),
    _   => Ok((parse_atom(first), rest)),
  }
}

fn read_sequence<'a>(tokens: &'a [String]) -> SResult<(Expression, &'a [String])> {
  let mut result: Vec<Expression> = vec![];
  let mut ts = tokens;

  loop {
    let (next, rest) = ts.split_first().ok_or(
      SErr::Reason("could not find closing `)`".to_string())
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

fn parse_list_of_floats(args: &[Expression]) -> SResult<Vec<f64>> {
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

fn parse_list_of_symbols(args: Rc<Expression>) -> SResult<Vec<String>> {
  let list = match args.as_ref() {
    Expression::List(l) => Ok(l.clone()),
    _ => Err(SErr::Reason(
      "expected args form to be a list".to_string()
    ))
  }?;
  list.iter()
    .map(|x| {
      match x {
        Expression::Symbol(s) => Ok(s.clone()),
        _ => Err(SErr::Reason(
          "expected symbols in the argument list".to_string()
        ))
      }
    })
    .collect()
}

macro_rules! comparison {
  ($check_fn:expr) => {{
    |args: &[Expression]| -> SResult<Expression> {
      let floats = parse_list_of_floats(args)?;
      let (first, rest) = floats.split_first().ok_or(
        SErr::Reason("expected at least one number".to_string())
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

fn init_env<'a>() -> Env<'a> {
  let mut operations: HashMap<String, Expression> = HashMap::new();
  
  operations.insert(
    "+".to_string(),
    Expression::Func(|args: &[Expression]| -> SResult<Expression> {
      let sum = parse_list_of_floats(args)?.iter().sum();
      Ok(Expression::Number(sum))
    })
  );
  
  operations.insert(
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

  operations.insert(
    "*".to_string(),
    Expression::Func(|args: &[Expression]| -> SResult<Expression> {
      let product = parse_list_of_floats(args)?.iter().product();
      Ok(Expression::Number(product))
    })
  );

  operations.insert(
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

  operations.insert(
    "=".to_string(),
    Expression::Func(comparison!(|a, b| a == b))
  );

  operations.insert(
    ">".to_string(),
    Expression::Func(comparison!(|a, b| a > b))
  );

  operations.insert(
    "<".to_string(),
    Expression::Func(comparison!(|a, b| a < b))
  );

  operations.insert(
    ">=".to_string(),
    Expression::Func(comparison!(|a, b| a >= b))
  );

  operations.insert(
    "<=".to_string(),
    Expression::Func(comparison!(|a, b| a <= b))
  );

  Env { operations, scope: None }
}

fn init_lambda_env<'a>(
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
      scope: Some(outer)
    }
  )
}

fn env_get(s: &str, env: &Env) -> Option<Expression> {
  match env.operations.get(s) {
    Some(expr) => Some(expr.clone()),
    None => {
      match &env.scope {
        Some(outer) => env_get(s, &outer),
        None => None
      }
    }
  }
}

fn eval(exp: &Expression, env: &mut Env) -> SResult<Expression> {
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

fn eval_forms(args: &[Expression], env: &mut Env) -> SResult<Vec<Expression>> {
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

fn parse_eval(input: String, env: &mut Env) -> SResult<Expression> {
  let (parsed, _) = parse(&tokenize(input))?;
  let evaluated = eval(&parsed, env)?;
  
  Ok(evaluated)
}

fn main() {
  let env = &mut init_env();
  
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
