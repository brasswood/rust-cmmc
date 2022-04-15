// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

extern crate pest;
extern crate get_pos_derive;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;
extern crate argh;

mod lex;
mod parse;
mod name;
mod type_check;
mod peg;
mod ast;
mod error;
mod ir;

use std::fs;
use std::process;
use std::io::{self, Write};
use argh::FromArgs;
use crate::ast::ProgramNode;

#[derive(FromArgs)]
/// cmm compiler
struct Args {
    #[argh(positional)]
    /// the input cmm file
    infile: String,

    #[argh(option, short = 't')]
    /// file to print tokens to
    tokens: Option<String>,

    #[argh(switch, short = 'p')]
    /// perform syntax checking
    parse: bool,

    #[argh(option, short='u')]
    /// unparse
    unparse: Option<String>,

    #[argh(option, short='n')]
    /// perform name analysis and output the result to <name>
    name: Option<String>,

    #[argh(switch, short='c')]
    /// typecheck
    typecheck: bool,

    #[argh(option, short='a')]
    /// translate to 3AC and output result to <file>
    threeac: Option<String>,
}

pub const SHORT_MAX: f64 = i16::MAX as f64;
pub const INT_MAX: f64 = i32::MAX as f64;

fn main() {
    let args: Args = argh::from_env();
    let maybe_input = fs::read_to_string(&args.infile);
    let contents = match maybe_input {
        Ok(s) => s,
        Err(_) => {
            writeln!(io::stderr(), "ERROR: Couldn't read {}", &args.infile).unwrap();
            process::exit(1);
        },
    };
    if let Some(outpath) = args.tokens {
        let outfile = match fs::File::create(&outpath) {
            Ok(f) => f,
            Err(_) => {
                writeln!(io::stderr(), "Could not open file {} for writing.", outpath).unwrap();
                process::exit(1);
            },
        };
        lex::lex(&contents, outfile);
    } 
    if args.parse {
        parse::parse(&contents);
    }
    if let Some(outpath) = args.unparse {
        let mut outfile = match fs::File::create(&outpath) {
            Ok(f) => f,
            Err(_) => {
                writeln!(io::stderr(), "Could not open file {} for writing.", outpath).unwrap();
                process::exit(1)
            },
        };
        let pair = parse::parse(&contents);
        let tree = ProgramNode::from(pair);
        parse::unparse(&tree, &mut outfile);
    }
    if let Some(outpath) = args.name {
        let mut outfile = match fs::File::create(&outpath) {
            Ok(f) => f,
            Err(_) => {
                writeln!(
                    io::stderr(),
                    "Could not open file {} for writing.", 
                    outpath,
                ).unwrap();
                process::exit(1)
            },
        };
        let pair = parse::parse(&contents);
        let mut tree = ProgramNode::from(pair);
        name::name_analysis(&mut tree, &mut outfile);
    }
    if args.typecheck {
        let pair = parse::parse(&contents);
        let mut tree = ProgramNode::from(pair);
        name::name_analysis_silent(&mut tree);
        type_check::type_check(&mut tree);
    }
    if let Some(outpath) = args.threeac {
        let mut outfile = match fs::File::create(&outpath) {
            Ok(f) => f,
            Err(_) => {
                writeln!(
                    io::stderr(),
                    "Could not open file {} for writing.", 
                    outpath,
                ).unwrap();
                process::exit(1)
            },
        };
        let pair = parse::parse(&contents);
        let mut tree = ProgramNode::from(pair);
        name::name_analysis_silent(&mut tree);
        type_check::type_check(&mut tree);
        ir::write_3ac(&tree, &mut outfile);
    }
}
