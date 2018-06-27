use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt;
use std::f64::consts;
use std::error::Error;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub enum ParseError {
    BorderError,
    UnknownTokenError(String),
    MissingParametersError(String),
    OddTokensError,
    EmptyInputError
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let message = match *self {
            ParseError::BorderError => "Incorrect borders".to_string(),
            ParseError::UnknownTokenError(ref token) => format!("Unknown token: {}", token),
            ParseError::MissingParametersError(ref token) => format!("Missing parameters for operator: {}", token),
            ParseError::OddTokensError => "Redundant tokens in input".to_string(),
            ParseError::EmptyInputError => "Empty input".to_string()
        };
        write!(f, "{}", message)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            ParseError::BorderError => "Incorrect borders",
            ParseError::UnknownTokenError(_) => "Unknown token",
            ParseError::MissingParametersError(_) => "Missing parameters",
            ParseError::OddTokensError => "Redundant tokens in input",
            ParseError::EmptyInputError => "Empty input"
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

type Link<T> = Rc<RefCell<Box<T>>>;

#[derive(Debug)]
pub struct AST {
    root: Link<Node>
}

enum Node {
    Variable,
    Number(f64),
    UnaryOperator(String, Link<Node>),
    BinaryOperator(String, Link<Node>, Link<Node>)
}

pub fn parse(input: &str) -> Result<AST, ParseError> {
    let mut stack = Vec::new();
    stack = input
        .split(" ")
        .try_fold(stack, |mut stack, token| {
            let unary_operators = vec!["sin", "cos", "tan", "ctg", "ln"];
            let binary_operators = vec!["+", "-", "*", "/", "^"];
            if let Ok(num) = token.parse::<f64>() {
                stack.push(AST::new(Node::Number(num)));
            }
            else if token == "x" {
                stack.push(AST::new(Node::Variable));
            }
            else if token == "pi" {
                stack.push(AST::new(Node::Number(consts::PI)));
            }
            else if token == "e" {
                stack.push(AST::new(Node::Number(consts::E)));
            }
            else if unary_operators.contains(&token) {
                let arg = stack.pop().ok_or(ParseError::MissingParametersError(token.to_owned()))?;
                stack.push(AST::new(Node::UnaryOperator(token.to_owned(), arg.root)));
            }
            else if binary_operators.contains(&token) {
                let left = stack.pop().ok_or(ParseError::MissingParametersError(token.to_owned()))?;
                let right = stack.pop().ok_or(ParseError::MissingParametersError(token.to_owned()))?;
                stack.push(AST::new(Node::BinaryOperator(token.to_owned(), left.root, right.root)));
            }
            else {
                Err(ParseError::UnknownTokenError(token.to_owned()))?
            }
            Ok(stack)
        })?;
    if stack.is_empty() {
        Err(ParseError::EmptyInputError)?
    }
    if stack.len() > 1 {
        Err(ParseError::OddTokensError)?
    }
    Ok(stack.pop().unwrap())
}

pub fn derivative(function: &AST) -> AST { unimplemented!(); }

impl AST {
    fn new(node: Node) -> AST {
        AST {
            root: Rc::new(RefCell::new(Box::new(node)))
        }
    }
}