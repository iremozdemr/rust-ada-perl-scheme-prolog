use std::collections::HashMap;
use std::io::{self, Write};

#[derive(Debug, Clone)]
enum Expr {
    Number(f64),
    Variable(String),
    Unary(char, Box<Expr>),
    Binary(Box<Expr>, char, Box<Expr>),
}

fn parse_expression(input: &str) -> Result<Expr, String> {
    let tokens = tokenize(input);
    let mut pos = 0;
    parse_expr(&tokens, &mut pos)
}

fn parse_expr(tokens: &[String], pos: &mut usize) -> Result<Expr, String> {
    let mut node = parse_term(tokens, pos)?;

    while *pos < tokens.len() {
        let op = &tokens[*pos];
        if op == "+" || op == "-" {
            *pos += 1;
            let rhs = parse_term(tokens, pos)?;
            node = Expr::Binary(Box::new(node), op.chars().next().unwrap(), Box::new(rhs));
        } else {
            break;
        }
    }

    Ok(node)
}

fn parse_term(tokens: &[String], pos: &mut usize) -> Result<Expr, String> {
    let mut node = parse_factor(tokens, pos)?;

    while *pos < tokens.len() {
        let op = &tokens[*pos];
        if op == "*" || op == "/" {
            *pos += 1;
            let rhs = parse_factor(tokens, pos)?;
            node = Expr::Binary(Box::new(node), op.chars().next().unwrap(), Box::new(rhs));
        } else {
            break;
        }
    }

    Ok(node)
}

fn parse_factor(tokens: &[String], pos: &mut usize) -> Result<Expr, String> {
    if *pos >= tokens.len() {
        return Err("Unexpected end of input".to_string());
    }

    let token = &tokens[*pos];

    if token == "-" {
        *pos += 1;
        let expr = parse_factor(tokens, pos)?;
        return Ok(Expr::Unary('-', Box::new(expr)));
    } else if token == "(" {
        *pos += 1;
        let expr = parse_expr(tokens, pos)?;
        if *pos >= tokens.len() || tokens[*pos] != ")" {
            return Err("Expected ')'".to_string());
        }
        *pos += 1;
        return Ok(expr);
    } else if let Ok(num) = token.parse::<f64>() {
        *pos += 1;
        return Ok(Expr::Number(num));
    } else if is_identifier(token) {
        *pos += 1;
        return Ok(Expr::Variable(token.to_string()));
    }

    Err(format!("Unexpected token: {}", token))
}

fn is_identifier(s: &str) -> bool {
    s.chars().all(|c| c.is_alphabetic())
}

fn eval(expr: &Expr, context: &HashMap<String, f64>) -> Result<f64, String> {
    match expr {
        Expr::Number(n) => Ok(*n),
        Expr::Variable(name) => context.get(name)
            .copied()
            .ok_or_else(|| format!("Undefined variable: {}", name)),
        Expr::Unary(op, expr) => {
            let val = eval(expr, context)?;
            match op {
                '-' => Ok(-val),
                _ => Err(format!("Unsupported unary operator: {}", op)),
            }
        }
        Expr::Binary(lhs, op, rhs) => {
            let l = eval(lhs, context)?;
            let r = eval(rhs, context)?;
            match op {
                '+' => Ok(l + r),
                '-' => Ok(l - r),
                '*' => Ok(l * r),
                '/' => {
                    if r == 0.0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(l / r)
                    }
                }
                _ => Err(format!("Unknown operator: {}", op)),
            }
        }
    }
}

fn main() {
    let mut vars: HashMap<String, f64> = HashMap::new();

    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() {
            println!("Error reading input");
            continue;
        }

        let line = line.trim();
        if line == "exit" {
            break;
        }

        if let Some(eq_pos) = line.find('=') {
            let (left, right) = line.split_at(eq_pos);
            let var_name = left.trim();
            let expr_str = right[1..].trim(); // after '='

            if var_name.is_empty() || !is_identifier(var_name) {
                println!("Invalid variable name");
                continue;
            }

            match parse_expression(expr_str) {
                Ok(expr) => match eval(&expr, &vars) {
                    Ok(value) => {
                        vars.insert(var_name.to_string(), value);
                        println!("{} = {}", var_name, value);
                    }
                    Err(e) => println!("Evaluation error: {}", e),
                },
                Err(e) => println!("Parse error: {}", e),
            }
        } else {
            match parse_expression(line) {
                Ok(expr) => match eval(&expr, &vars) {
                    Ok(value) => println!("{}", value),
                    Err(e) => println!("Evaluation error: {}", e),
                },
                Err(e) => println!("Parse error: {}", e),
            }
        }
    }
}

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();

    for c in input.chars() {
        if c.is_whitespace() {
            continue;
        } else if "+-*/=()".contains(c) {
            if !current.is_empty() {
                tokens.push(current.clone());
                current.clear();
            }
            tokens.push(c.to_string());
        } else {
            current.push(c);
        }
    }

    if !current.is_empty() {
        tokens.push(current);
    }

    tokens
}