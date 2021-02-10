use crate::types::Expr;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum EvalResult {
    Err(String),
    Expr(Rc<Expr>),
    Unit,
}

#[derive(Debug)]
pub struct Environment {
    pub contexts: Vec<HashMap<String, (Vec<String>, Rc<Expr>)>>,
}

impl Environment {
    pub fn empty() -> Environment {
        Environment {
            contexts: Vec::new(),
        }
    }

    pub fn from_vars(vars: &[(&str, Rc<Expr>)]) -> Environment {
        let mut env = Environment::empty();
        env.push_context();
        vars.iter().for_each(|(name, expr)| {
            let _ = env.add_var(name, expr.clone());
        });
        env
    }

    pub fn default() -> Environment {
        let defaults: HashMap<String, (Vec<String>, Rc<Expr>)> = [
            ("False".into(), (Vec::new(), Expr::list(&[]))),
            ("True".into(), (Vec::new(), Expr::list(&[Expr::fnum(1.0)]))),
        ].iter().cloned().collect();

        Environment{
            contexts: vec![defaults],
        }
    }

    /// Looks up the given symbol in the Environment.
    pub fn lookup(&self, symbol: &str) -> Option<(Vec<String>, Rc<Expr>)> {
        self.contexts
            .iter()
            .rev()
            .find(|ctx| ctx.contains_key(symbol))
            .map(|ctx| ctx.get(symbol))
            .flatten()
            .cloned()
    }

    /// Checks whether the given symbol exists in the Environment.
    pub fn contains_key(&self, symbol: &str) -> bool {
        self.contexts
            .iter()
            .rev()
            .find(|ctx| ctx.contains_key(symbol))
            .is_some()
    }

    /// Pushes a new context on the `contexts` stack.
    pub fn push_context(&mut self) {
        self.contexts.push(HashMap::new());
    }

    /// Pops the last context from the `contexts` stack.
    pub fn pop_context(&mut self) {
        self.contexts.pop();
    }

    /// Adds a variable definition to the Environment
    pub fn add_var(&mut self, var: &str, val: Rc<Expr>) -> Result<(), String> {
        self.contexts
            .last_mut()
            .map_or_else(
                || Err("Environment does not have any context to add to.".into()),
                |ctx| { ctx.insert(var.to_string(), (Vec:: new(), val.clone())); Ok(()) },
            )
    }

    /// Adds a function definition to the Environment
    pub fn add_fn(&mut self, name: &str, params: &[String], body: Rc<Expr>) -> Result<(), String> {
        self.contexts.last_mut().map_or(
            Err("Environment does not have a context to add to.".into()),
            |ctx| {
                let param_names: Vec<String> = params.iter().map(|s| s.to_string()).collect();
                ctx.insert(name.to_string(), (param_names, body.clone()));
                Ok(())
            }
        )
    }

    pub fn num_contexts(&self) -> usize {
        self.contexts.len()
    }
}

/// Generates the output printed to standard out when the user calls print.
pub fn gen_print_output(expr: Rc<Expr>, env: &mut Environment) -> String {
    match &*expr{
        Expr::Symbol(s) => {
            match env.lookup(&s){
                None => s.to_string(),
                Some((params, e)) if params.len() == 0 => gen_print_output(e,env),
                _ => format!("<func-object: {}>", s.to_string()),
            }
        },
        Expr::FNum(n) => format!("{}",n),
        Expr::List(vals) => {
            let vals_out: Vec<String> = vals.iter()
                .cloned()
                .map(|x| gen_print_output(x, env))
                .collect();
            format!("({})", vals_out.join(" "))
        }
    }
}

fn evaluate_symbol(expr: Rc<Expr>, sym: &str, args: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    env.lookup(sym).map_or_else(
            || EvalResult::Expr(expr),
            |(param_names, expression)| {
                if param_names.is_empty() {
                    println!("Evaluating {:?}, lookup returned {:?}", sym, expression);
                    eval(expression.clone(), env)
                } else {
                    if args.len() != param_names.len() {
                        return EvalResult::Err(format!("Provided {} arguments but expected {}",
                            args.len(),
                            param_names.len()
                        ));
                    }
                    let mapped_args: Result<Vec<(String, Rc<Expr>)>, String> = args
                        .iter()
                        .zip(param_names)
                        .map(|(expr, name)| match eval(expr.clone(), env) {
                            EvalResult::Expr(e) => Ok((name.to_string(), e.clone())),
                            EvalResult::Err(err) => Err(err),
                            _ => Err("Cannot pass Unit as an argument to a function.".into()),
                        })
                        .collect();
                    env.push_context();

                    let result = mapped_args.map_or_else(
                        |e| EvalResult::Err(e),
                        |args_tuples| {
                            args_tuples.iter().for_each(|(name, expr)| {
                                let _ = env.add_var(name, expr.clone());
                            });
                            eval(expression.clone(), env)
                        },
                    );
                    env.pop_context();
                    result
                }
            }
    )
}

fn add_var_to_env(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult{
    if vals.len() != 2 {
        EvalResult::Err("Invalid variable definition. Should look like (let someVar someExpr)".into());
    }

    match (&*vals[0], &vals[1]){
        (Expr::Symbol(s), e) => match eval(e.clone(),env){
            EvalResult::Expr(e) => env
                .add_var(s,e)
                .map_or_else(|s| EvalResult::Err(s), |_| EvalResult::Unit),
            EvalResult::Unit => EvalResult::Err("Cannot assign unit to a variable.".into()),
            err => err,

        },
        _ => EvalResult::Err(
            "Second element of variable definition must be a symbol and third must be an expression.".into(),
        ),
    }
}

fn add_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty() {
        return EvalResult::Err("Must perform addition on at least one number".into());
    }
    let total = vals
        .iter()
        .map(|e| match eval(e.clone(),env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => Ok(*n),
                _ => Err("Can only sum numbers.".into()),
            },
            _ => Err("Can only sum numbers.".into()),
        })
        .collect::<Result<Vec<f64>, String>>();
    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| EvalResult::Expr(Expr::fnum(xs.iter().sum())),
    )
}

fn sub_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty() {
        return EvalResult::Err("Must perform subtraction on at least one number".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(),env) {
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => Ok(*n),
                _ => Err("Can only subtract numbers.".into()),
            },
            _ => Err("Can only subtract numbers.".into()),
        })
        .collect::<Result<Vec<f64>, String>>();

    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| if xs.len() == 1 {
            EvalResult::Expr(Expr::fnum(xs.iter().fold(0.0, |mut acc, &x| {acc -= x; acc})))
        } else {
            EvalResult::Expr(Expr::fnum(xs.iter().skip(1).fold(xs[0], |mut acc, &x| {acc -= x; acc})))
        },
    )
}

fn mul_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty(){
        return EvalResult::Err("Must perform multiplication on at least one number".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(), env){
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => Ok(*n),
                _ => Err("Can only mutiply numbers.".into()),
            },
            _ => Err("Can only mutiply numbers.".into()),
        })
        .collect::<Result<Vec<f64>, String>>();
    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| EvalResult::Expr(Expr::fnum(xs.iter().product())),
    )
}

fn div_vals(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty(){
        return EvalResult::Err("Must perform division on at least one number".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(), env){
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(n) => Ok(*n),
                _ => Err("Can only divide numbers.".into()),
            },
            _ => Err("Can only divide numbers.".into()),
        })
        .collect::<Result<Vec<f64>, String>>();
    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| if xs.len() == 1 {
            EvalResult::Expr(Expr::fnum(xs.iter().fold(0.0, |mut acc, &x| {acc /= x; acc})))
        } else {
            EvalResult::Expr(Expr::fnum(xs.iter().skip(1).fold(xs[0], |mut acc, &x| {acc /= x; acc})))
        },
    )
}

fn or(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty(){
        return EvalResult::Err("Must perform on at least one value".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(), env){
            EvalResult::Expr(exp) => match &*exp {
                Expr::Symbol(s) => if s == "True" {
                    Ok("True".to_string())
                } else {
                    Ok("False".to_string())
                },
                Expr::List(vs) => if vs.len() == 0 {
                    Ok("False".to_string())
                } else {
                    Ok("True".to_string())
                },
                _ => Err("Can only perform on boolean values.".into()),
            },
            _ => Err("Can only perform on boolean values.".into()),
        })
        .collect::<Result<Vec<String>, String>>();
    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| if xs.iter().any(|x| x == "True") {
            EvalResult::Expr(Expr::symbol("True"))
        } else {
            EvalResult::Expr(Expr::symbol("False"))
        },
    )
}

fn and(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty(){
        return EvalResult::Err("Must perform on at least one value".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(), env){
            EvalResult::Expr(exp) => match &*exp {
                Expr::Symbol(s) => if s == "True" {
                    Ok("True".to_string())
                } else {
                    Ok("False".to_string())
                },
                Expr::List(vs) => if vs.len() == 0 {
                    Ok("False".to_string())
                } else {
                    Ok("True".to_string())
                },
                _ => Err("Can only perform on boolean values.".into()),
            },
            _ => Err("Can only perform on boolean values.".into()),
        })
        .collect::<Result<Vec<String>, String>>();
    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| if xs.iter().any(|x| x == "False") {
            EvalResult::Expr(Expr::symbol("False"))
        } else {
            EvalResult::Expr(Expr::symbol("True"))
        },
    )
}

fn not(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty(){
        return EvalResult::Err("Must perform on at least one value".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(), env){
            EvalResult::Expr(exp) => match &*exp {
                Expr::Symbol(s) => if s == "True" {
                    Ok("False".to_string())
                } else {
                    Ok("True".to_string())
                },
                Expr::List(vs) => if vs.len() == 0 {
                    Ok("True".to_string())
                } else {
                    Ok("False".to_string())
                },
                _ => Err("Can only perform on boolean values.".into()),
            },
                _ =>Err("Can only perform on boolean values.".into()),
        })
        .collect::<Result<Vec<String>, String>>();
    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| EvalResult::Expr(Expr::symbol(&xs[0])),
    )
}

fn eq(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty() || vals.len() == 1 {
        return EvalResult::Err("Must perform on at least two values".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(), env){
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(f) => Ok(f.to_string()),
                Expr::Symbol(s) => Ok(s.to_string()),
                Expr::List(vs) => Ok(vs.iter().map(|e1| match eval (e1.clone(), env) {
                    EvalResult::Expr(exp1) => match &*exp1 {
                        Expr::FNum(f) => f.to_string(),
                        Expr::Symbol(s) => s.to_string(),
                        _ => "Error".to_string(), 
                    },
                    _ => "Error".to_string(),
                })
                .collect::<String>()),
            },
                _ => Err("Can only perform on boolean values.".into()),
        })
        .collect::<Result<Vec<String>, String>>();

    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| if xs.iter().all(|x| x == &xs[0]) {
            EvalResult::Expr(Expr::symbol("True"))
        } else {
            EvalResult::Expr(Expr::symbol("False"))
        }
    )
}

fn neq(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.is_empty() || vals.len() == 1 {
        return EvalResult::Err("Must perform on at least two values".into());
    }

    let total = vals
        .iter()
        .map(|e| match eval(e.clone(), env){
            EvalResult::Expr(exp) => match &*exp {
                Expr::FNum(f) => Ok(f.to_string()),
                Expr::Symbol(s) => Ok(s.to_string()),
                Expr::List(vs) => Ok(vs.iter().map(|e1| match eval (e1.clone(), env) {
                    EvalResult::Expr(exp1) => match &*exp1 {
                        Expr::FNum(f) => f.to_string(),
                        Expr::Symbol(s) => s.to_string(),
                        _ => "Error".to_string(),
                    },
                    _ => "Error".to_string(),
                })
                .collect::<String>()),
            },
                _ => Err("Can only perform on boolean values.".into()),
        })
        .collect::<Result<Vec<String>, String>>();

    total.map_or_else(
        |err| EvalResult::Err(err),
        |xs| if xs.iter().all(|x| x == &xs[0]) {
            EvalResult::Expr(Expr::symbol("False"))
        } else {
            EvalResult::Expr(Expr::symbol("True"))
        }
    )
}

fn if_then_else(blocks: &[Rc<Expr>], env: &mut Environment) -> EvalResult{
    if blocks.len() != 3 {
        return EvalResult::Err("If Expressions must have the format (if (<prediacte block>) (<then block>) (<else block>))".into());
    }
    match eval(blocks[0].clone(), env){
        EvalResult::Expr(expr) => {
            match &*expr {
                Expr::List(vs) if vs.len() == 0 => eval(blocks[2].clone(),env),
                _ => eval(blocks[1].clone(), env),
            }
        },
        EvalResult::Unit => EvalResult::Err("If expression predicates must return an expression.".into()),
        err => err
    }
}

fn add_fn_to_env(vals: &[Rc<Expr>], env: &mut Environment) -> EvalResult {
    if vals.len() != 3{
        return EvalResult::Err("Function definitions must follow the pattern of (fn fn-name (arg1 arg2 arg3 .. argn) <Expr>".into());
    }
    let fn_name = &*vals[0];
    let p_names = &*vals[1];
    let body = &vals[2];
    match (&*fn_name, p_names, body){
        (Expr::Symbol(fn_name), Expr::List(params), body) => {
            let ps: Result<Vec<String>, String> = params.iter().cloned().map(|e| {
                if let Expr::Symbol(n) = &*e {
                    Ok(n.to_string())
                }else{
                    Err("Function parameters must be symbols.".into())
                }
            })
            .collect();

            ps.map_or_else(
                |err| EvalResult::Err(err),
                |xs| env.add_fn(fn_name, xs.as_slice(), body.clone()).map_or_else(
                    |err| EvalResult::Err(err),
                    |_| EvalResult::Unit
                )
            )
        },
        _ => EvalResult::Err("Function definitions must follow the pattern (fn fn-name (arg1 arg2 arg3 .. argn) <Expr>".into()),
    }
    
}

/// Evaluates the given expression.
pub fn eval(e: Rc<Expr>, env: &mut Environment) -> EvalResult {
    match &*e {
        Expr::FNum(_) => EvalResult::Expr(e.clone()),
        Expr::Symbol(s) => evaluate_symbol(e.clone(), s, &[], env),
        Expr::List(vals) => {
            if vals.is_empty(){
                return EvalResult::Expr(Expr::list(&[]));
            }
            let op = &*vals[0];
            match op{
                Expr::Symbol(s) if s == "+" => add_vals(&vals[1..], env),

                Expr::Symbol(s) if s == "-" => sub_vals(&vals[1..], env),

                Expr::Symbol(s) if s == "*" => mul_vals(&vals[1..], env),

                Expr::Symbol(s) if s == "/" => div_vals(&vals[1..], env),

                Expr::Symbol(s) if s == "or" => or(&vals[1..], env),

                Expr::Symbol(s) if s == "and" => and(&vals[1..], env),

                Expr::Symbol(s) if s == "not" => not(&vals[1..], env),

                Expr::Symbol(s) if s == "=" => eq(&vals[1..], env),
                
                Expr::Symbol(s) if s == "!=" => neq(&vals[1..], env),

                Expr::Symbol(s) if s == "let" => add_var_to_env(&vals[1..], env),

                Expr::Symbol(s) if s == "fn" => add_fn_to_env(&vals[1..], env),

                Expr::Symbol(s) if s == "print" => {
                    let output: Vec<String> = vals[1..].iter()
                        .cloned()
                        .map(|expr| gen_print_output(expr, env))
                        .collect();
                    println!("{}",output.join(" "));
                    EvalResult::Unit
                },
                Expr::Symbol(s) if s == "if" => if_then_else(&vals[1..], env),
                Expr::Symbol(s) if env.contains_key(&s) => {
                    evaluate_symbol(e.clone(), s, &vals[1..],env)
                }

                _ => {
                    let res: Result<Vec<Rc<Expr>>, EvalResult> = vals.iter()
                        .cloned()
                        .map(|expr| eval(expr,env))
                        .filter(|x| *x != EvalResult::Unit)
                        .map(|x| if let EvalResult::Expr(expr) = x {
                            Ok(expr)
                        }else{
                            Err(x)
                        })
                        .collect();
                    res.map_or_else(
                        |err| err,
                        |exprs| EvalResult::Expr(Expr::list(&exprs))
                    )
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
