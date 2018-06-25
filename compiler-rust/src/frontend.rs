use std::fmt::Display;
use std::fmt::Formatter;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum ParseError {
    BorderError,
    _UnknownTokenError(String),
    _MissingParametersError(String),
    _OddTokensError,
    _EmptyInputError
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let message = match *self {
            ParseError::BorderError => "Incorrect borders".to_string(),
            ParseError::_UnknownTokenError(ref token) => format!("Unknown token: {}", token),
            ParseError::_MissingParametersError(ref token) => format!("Missing parameters for operator: {}", token),
            ParseError::_OddTokensError => "Redundant tokens in input".to_string(),
            ParseError::_EmptyInputError => "Empty input".to_string()
        };
        write!(f, "{}", message)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            ParseError::BorderError => "Incorrect borders",
            ParseError::_UnknownTokenError(_) => "Unknown token",
            ParseError::_MissingParametersError(_) => "Missing parameters",
            ParseError::_OddTokensError => "Redundant tokens in input",
            ParseError::_EmptyInputError => "Empty input"
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}