use crate::peg::{Rule, CMMParser};
use pest::Parser;
use std::io::{self, Write};
use std::process;

pub fn parse(input: &str) {
    match CMMParser::parse(Rule::program, input) {
        Ok(_) => (),
        Err(_) => {
            writeln!(io::stderr(), "syntax error\nParse failed").unwrap();
            process::exit(1);
        },
    }
}
