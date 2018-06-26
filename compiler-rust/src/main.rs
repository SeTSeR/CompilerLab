use std::env;
use std::process;

extern crate compiler_rust;

use compiler_rust::Config;

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = Config::new(&args).unwrap_or_else(|err| {
        eprintln!("There was a problem during parsing arguments: {}", err);
        process::exit(1);
    });

    if let Err(e) = compiler_rust::run(config) {
        eprintln!("There were an errors during compilation: {}", e);
        process::exit(1);
    }
}
