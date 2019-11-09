use std::fmt;

#[derive(Debug)]
pub enum SErr {
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

pub type SResult<T> = Result<T, SErr>;