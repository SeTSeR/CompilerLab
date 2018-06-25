use std::error::Error;
use std::fs::File;

pub struct Config {
    pub inputfile: String,
    pub outfile: String,
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

pub fn run(config: Config) -> Result<(), Box<Error>> {
    let mut infile = File::open(config.inputfile);

    let mut content = String::new();
    f.read_to_string(&mut content)?;

    Ok(())
}