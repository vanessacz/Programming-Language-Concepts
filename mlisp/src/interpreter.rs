use crate::lex::lex;
use crate::parse::parse;
use crate::eval::{eval, Environment, EvalResult};

/// Lexes, parses, and evaluates the given program.
pub fn run_interpreter(program: &str) -> EvalResult {
	match lex(&program) {
		Err(err) => EvalResult::Err("Lex error".into()),
		Ok(tokens) => match parse(&tokens) {
			Err(err) => EvalResult::Err("Parse error".into()),
			Ok(expr) => {
				let mut env = Environment::default();
				match eval(expr.clone(), &mut env){
					EvalResult::Expr(n) => EvalResult::Expr(n),
					_ => EvalResult::Err("Eval error".into())
				}
			}
		}
	}
}
