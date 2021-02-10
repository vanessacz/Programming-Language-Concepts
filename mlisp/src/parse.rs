use crate::lex::Token;
use crate::types::Expr;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParseError {
    BadParse(String),
    EOF,
}

#[derive(Debug)]
pub enum ParseResult {
    Success(usize, Rc<Expr>),
    Failure(ParseError),
}

pub fn parse(tokens: &[Token]) -> Result<Rc<Expr>, ParseError> {
    match parser(tokens, 0) {
    	ParseResult::Success(_, expr) => Ok(expr),
    	ParseResult::Failure(err) => Err(err),
    }
}

fn parser(tokens: &[Token], index: usize) -> ParseResult {
	let mut index = index;
	if let Some(mut t) = tokens.get(index) {
		match &*t {
			Token::LPar => {
				index += 1;
				let mut exprs = Vec::new();

				while *t !=Token::RPar {
					match parser(tokens, index) {
						ParseResult::Success(idx, expr) => {
							exprs.push(expr);
							index = idx;
						},
						e => return e,
					}
					if index >= tokens.len() {
						return ParseResult::Failure(ParseError::BadParse("Unclosed delimeter".into()));
					}
					t = &tokens[index];
				}
				ParseResult::Success(index + 1, Expr::list(&exprs))
			},
			Token::RPar => {
				ParseResult::Failure(ParseError::BadParse("Unexpected ) encountered.".to_string()))
			},
			Token::Literal(s) => {
				if let Ok(n) = s.parse::<f64>() {
					ParseResult::Success(index + 1, Expr::fnum(n))
				} else {
					ParseResult::Success(index + 1, Expr::symbol(&s))
				}
			},
			_ => ParseResult::Failure(ParseError::BadParse(format!("Unknown token: {:?}", *t))),
		}

	} else {
		ParseResult::Failure(ParseError::EOF)
	}
}

#[cfg(test)]
mod test {
    use super::*;
}
