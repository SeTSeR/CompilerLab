use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;

mod frontend;

use frontend::AST;
use frontend::ParseError;

pub struct Config {
    pub inputfile: String,
    pub outfile: String,
}

#[derive(Debug)]
enum CliError {
    Parse(frontend::ParseError),
    IO(io::Error)
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CliError::IO(ref err) => write!(f, "IO error: {}", err),
            CliError::Parse(ref err) => write!(f, "Parse error: {}", err)
        }
    }
}

impl Error for CliError {
    fn description(&self) -> &str {
        match *self {
            CliError::IO(ref err) => err.description(),
            CliError::Parse(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            CliError::IO(ref err) => Some(err),
            CliError::Parse(ref err) => Some(err)
        }
    }
}

impl From<io::Error> for CliError {
    fn from(err: io::Error) -> Self {
        CliError::IO(err)
    }
}

impl From<frontend::ParseError> for CliError {
    fn from(err: frontend::ParseError) -> Self {
        CliError::Parse(err)
    }
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 3 {
            return Err("There should be at least two arguments: input file name and output file name");
        }
        let ifile = args[1].clone();
        let ofile = args[2].clone();
        Ok(Config { inputfile: ifile, outfile: ofile })
    }
}

fn collect(acc: Result<Vec<AST>, CliError>, line: &str) -> Result<Vec<AST>, CliError> {
    let mut vec = acc?;
    vec.push(frontend::parse(line)?);
    Ok(vec)
}

pub fn run(config: Config) -> Result<(), Box<Error>> {
    let mut infile = File::open(config.inputfile)?;

    let mut content = String::new();
    infile.read_to_string(&mut content)?;

    let mut lines = content.lines();

    let bounds: Vec<f64> = lines.next()
        .ok_or_else(|| ParseError::BorderError)?
        .split(" ")
        .take(2)
        .map(|str| str.parse())
        .take_while(Result::is_ok)
        .map(Result::unwrap)
        .collect();

    if bounds.len() < 2 {
        Err(ParseError::BorderError)?
    }

    let (_a, _b) = (bounds[0], bounds[1]);

    let functions = lines.fold(Ok(Vec::new()), collect)?;
    let _derivatives: Vec<AST> = functions.iter().map(|tree| frontend::derivative(&tree)).collect();

    Ok(())
}