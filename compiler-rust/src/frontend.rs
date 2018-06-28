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

pub trait Monoid {
    fn mempty() -> Self;
    fn mappend(&mut self, b: Self) -> ();
}

pub type Link<T> = Rc<RefCell<Box<T>>>;

#[derive(Debug, Clone)]
pub struct AST {
    root: Link<Node>
}

#[derive(Debug, Clone)]
pub enum Node {
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
                let right = stack.pop().ok_or(ParseError::MissingParametersError(token.to_owned()))?;
                let left = stack.pop().ok_or(ParseError::MissingParametersError(token.to_owned()))?;
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

fn pack(node: Node) -> Link<Node> {
    Rc::new(RefCell::new(Box::new(node)))
}

fn __derivative(function: &Link<Node>) -> Node {
    match *(*function.borrow()) {
        Node::Variable => Node::Number(1.0),
        Node::Number(_) => Node::Number(0.0),
        Node::UnaryOperator(ref token, ref arg) => {
            match token.as_str() {
                "sin" => Node::BinaryOperator("*".to_string(),
                pack(Node::UnaryOperator("cos".to_string(), arg.clone())),
                pack(__derivative(arg))),

                "cos" => Node::BinaryOperator("*".to_string(),
                 pack(Node::Number(-1.0)),
                 pack(Node::BinaryOperator("*".to_string(),
                    pack(Node::UnaryOperator("sin".to_string(), arg.clone())),
                    pack(__derivative(arg))))),

                "tan" => Node::BinaryOperator("/".to_string(),
                pack(__derivative(arg)),
                pack(Node::BinaryOperator("*".to_string(),
                    pack(Node::UnaryOperator("cos".to_string(), arg.clone())),
                    pack(Node::UnaryOperator("cos".to_string(), arg.clone()))))),

                "ctg" => Node::BinaryOperator("/".to_string(),
                pack(Node::BinaryOperator("*".to_string(),
                    pack(Node::Number(-1.0)),
                    pack(__derivative(arg)))),
                pack(Node::BinaryOperator("*".to_string(),
                    pack(Node::UnaryOperator("sin".to_string(), arg.clone())),
                    pack(Node::UnaryOperator("sin".to_string(), arg.clone()))))),

                "ln" => Node::BinaryOperator("/".to_string(),
                pack(__derivative(arg)),
                arg.clone()),

                _ => Node::Variable
            }
        },
        Node::BinaryOperator(ref token, ref left, ref right) => {
            match token.as_str() {
                "+" => Node::BinaryOperator("+".to_string(),
                pack(__derivative(left)),
                pack(__derivative(right))),

                "-" => Node::BinaryOperator("-".to_string(),
                pack(__derivative(left)),
                pack(__derivative(right))),

                "*" => Node::BinaryOperator("+".to_string(),
                pack(Node::BinaryOperator("*".to_string(),
                    pack(__derivative(left)),
                    right.clone())),
                pack(Node::BinaryOperator("*".to_string(),
                    left.clone(),
                    pack(__derivative(right))))),

                "/" => Node::BinaryOperator("/".to_string(),
                pack(Node::BinaryOperator("+".to_string(),
                    pack(Node::BinaryOperator("*".to_string(),
                        pack(__derivative(left)),
                        right.clone())),
                    pack(Node::BinaryOperator("*".to_string(),
                        left.clone(),
                        pack(__derivative(right)))))),
                pack(Node::BinaryOperator("*".to_string(),
                    right.clone(),
                    right.clone()))),

                "^" => Node::BinaryOperator("*".to_string(),
                pack(Node::BinaryOperator("^".to_string(),
                    left.clone(),
                    right.clone())),
                pack(__derivative(&pack(Node::BinaryOperator("*".to_string(),
                    right.clone(),
                    pack(Node::UnaryOperator("ln".to_string(), left.clone()))))))),

                _ => Node::Variable
            }
        }
    }
}

pub fn derivative(function: &AST) -> AST {
    AST::new(__derivative(&function.root))
}

fn collect_info<T: Monoid, F>(root: &Link<Node>, visit_node: &F) -> T
    where F: Fn(&Link<Node>) -> T {
    let mut info = T::mempty();
    match *(*root.borrow()) {
        Node::UnaryOperator(_, ref arg) => info.mappend(collect_info(arg, visit_node)),
        Node::BinaryOperator(_, ref left, ref right) => {
            info.mappend(collect_info(left, visit_node));
            info.mappend(collect_info(right, visit_node));
        }
        _ => ()
    }
    info.mappend(visit_node(root));
    info
}

impl AST {
    fn new(node: Node) -> AST {
        AST {
            root: Rc::new(RefCell::new(Box::new(node)))
        }
    }

    pub fn collect_info<T: Monoid, F>(&self, visit_node: &F) -> T
        where F: Fn(&Link<Node>) -> T {
        collect_info(&self.root, visit_node)
    }
}