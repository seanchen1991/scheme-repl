use std::vec::IntoIter;
use std::iter::Peekable;

trait GentleIterator<I: Iterator> {
  fn take_until<F>(&mut self, pred: F) -> IntoIter<I::Item>
    where F: Fn(&I::Item) -> bool;
}

impl<I: Iterator> GentleIterator<I> for Peekable<I> {
  fn take_until<F>(&mut self, pred: F) -> IntoIter<I::Item>
    where F: Fn(&I::Item) -> bool 
  {
    let mut v: Vec<I::Item> = vec![];
    while self.peek().map_or(false, &pred) {
      v.push(self.next().unwrap());
    }

    v.into_iter()
  }
}

pub fn tokenize<I>(iter: &mut Peekable<I>) -> Vec<String> 
  where I: Iterator<Item=char>
{
  let mut tokens: Vec<String> = vec![];

  while let Some(t) = tokenize_single(iter) {
    tokens.push(t);
  }

  tokens
}

fn tokenize_single<I>(iter: &mut Peekable<I>) -> Option<String>
  where I: Iterator<Item=char>
{
  while skip_whitespace(iter) || skip_comment(iter) {
    continue;
  }

  check_lparen(iter)
    .or_else(|| check_rparen(iter))
    .or_else(|| check_string(iter))
    .or_else(|| check_hash(iter))
    .or_else(|| check_symbol(iter))
}

fn skip_whitespace<I>(iter: &mut Peekable<I>) -> bool 
  where I: Iterator<Item=char>
{
  if check_char(iter, ' ') || check_char(iter, '\n') {
    iter.next();
    true
  } else {
    false
  }
}

fn skip_comment<I>(iter: &mut Peekable<I>) -> bool
  where I: Iterator<Item=char>
{
  if check_char(iter, ';') {
    iter.take_until(|c| *c != '\n');
    true
  } else {
    false
  }
}

fn check_lparen<I>(iter: &mut Peekable<I>) -> Option<String>
  where I: Iterator<Item=char>
{
  check_for(iter, '(')
}

fn check_rparen<I>(iter: &mut Peekable<I>) -> Option<String>
  where I: Iterator<Item=char>
{
  check_for(iter, ')')
}

fn check_string<I>(iter: &mut Peekable<I>) -> Option<String>
  where I: Iterator<Item=char>
{
  if !check_char(iter, '"') {
    return None;
  } 

  iter.next();
  let value = iter.take_until(|c| *c != '"').collect();
  iter.next();

  Some(value)
}

fn check_hash<I>(iter: &mut Peekable<I>) -> Option<String>
  where I: Iterator<Item=char>
{
  if !check_char(iter, '#') {
    return None;
  }

  iter.next();
  match iter.next() {
    Some('t') => Some("#t".to_string()),
    Some('f') => Some("#f".to_string()),
    Some(c)   => panic!("Expected '#t' or '#f', got: '#{}'", c),
    None      => panic!("Expected char after '#', none found")
  }
}

fn check_symbol<I>(iter: &mut Peekable<I>) -> Option<String>
  where I: Iterator<Item=char>
{
  if !check(iter, |_| true) {
    return None;
  }

  let value: String = iter.take_until(|c| *c != ' ' && *c != ')' && *c != '\n').collect();

  match value.parse::<f32>() {
      Ok(n)  => Some(n.to_string()),
      Err(_) => Some(value)
  }
}

fn check_for<I>(iter: &mut Peekable<I>, chr: char) -> Option<String>
  where I: Iterator<Item=char>
{
  if !check_char(iter, chr) {
    return None;
  }

  iter.next();
  Some(chr.to_string())
}

fn check<F, I>(iter: &mut Peekable<I>, f: F) -> bool 
  where F: Fn(char) -> bool,
        I: Iterator<Item=char>
{
  if let Some(&x) = iter.peek() {
    f(x)
  } else {
    false
  }
}

fn check_char<I>(iter: &mut Peekable<I>, chr: char) -> bool 
  where I: Iterator<Item=char>
{
  check(iter, |x| x == chr)
}