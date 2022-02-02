extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::fs;
use std::env;
use std::process;
use std::io::{self, Write};

#[derive(Parser)]
#[grammar = "cmm.pest"]
pub struct CMMParser;

const SHORT_MAX: f64 = i16::MAX as f64;
const INT_MAX: f64 = i32::MAX as f64;

thread_local!(static PROG_NAME: String = env::args().next().unwrap());
fn help_and_die() {
    PROG_NAME.with(|name| {println!("Usage: {} <infile.cmm> -t <tokens.txt>", name);});
    process::exit(1);
}

fn main() {
    let mut args = env::args();
    let mut outpath = String::new();
    let mut inpath = String::new();
    args.next(); // Consume one arg so that we get the program name out of the way
    while let Some(arg) = args.next() {
        if arg.chars().next() == Some('-') {
            // parse an option
            match &arg[1..] {
                "t" => {
                    if outpath.len() == 0 {
                        let next_arg = args.next();
                        match next_arg {
                            Some(s) => outpath = s,
                            None => help_and_die(),
                        }
                    } else {
                        writeln!(io::stderr(), "ERROR: Used -t more than once.").unwrap();
                        help_and_die();
                    }
                },
                _ => {
                    writeln!(io::stderr(), "ERROR: Unknown option {}", arg).unwrap();
                    help_and_die();
                },
            }
        } else {
            if inpath.len() == 0 {
                inpath = arg;
            } else {
                writeln!(io::stderr(), "ERROR: Too many input files specified").unwrap();
                help_and_die();
            }
        }
    }

    if inpath.len() == 0 {
        writeln!(io::stderr(), "ERROR: No input file specified.").unwrap();
        help_and_die();
    }

    if outpath.len() == 0 {
        writeln!(io::stderr(), "ERROR: No tokens file specified.").unwrap();
        help_and_die();
    }

    let maybe_input = fs::read_to_string(&inpath);
    let contents;
    match maybe_input {
        Ok(s) => contents = s,
        Err(_) => {
            writeln!(io::stderr(), "ERROR: Couldn't read {}", inpath).unwrap();
            process::exit(1);
        },
    }

    let maybe_all_pairs = CMMParser::parse(Rule::file, &contents);
    let mut all_pairs; // Pairs
    match maybe_all_pairs {
        Ok(p) => all_pairs = p,
        Err(e) => {
            writeln!(io::stderr(), "CRITICAL ERROR: Parser failed.").unwrap();
            writeln!(io::stderr(), "Here is the Error struct returned by pest:").unwrap();
            writeln!(io::stderr(), "{:?}", e).unwrap();
            process::exit(1);
        }
    }
    let file_pair = all_pairs.next().unwrap(); // Pair
    let token_pairs = file_pair.into_inner(); // Pairs
    let mut outfile = match fs::File::create(&outpath) {
        Ok(f) => f,
        Err(_) => {
            writeln!(io::stderr(), "Could not open file {} for writing.", outpath).unwrap();
            process::exit(1);
        },
    };
    for token_pair in token_pairs {
        let rule = token_pair.as_rule();
        match rule {
            Rule::COM => (), // ignore comment (had to make this atomic)
            Rule::reserved => { // go into reserved (also had to make this atomic)
                let reserved_token_pair = token_pair.into_inner().next().unwrap();
                write_token(&mut outfile, reserved_token_pair);
            },
            Rule::ERROR => {
                let error_token_pair = token_pair.into_inner().next().unwrap();
                let (start_line, start_col) = error_token_pair.as_span()
                    .start_pos()
                    .line_col();
                let (end_line, end_col) = error_token_pair.as_span()
                    .end_pos()
                    .line_col();
                write!(io::stderr(), "FATAL [{},{}]-[{},{}]: ", start_line, start_col, end_line, end_col).unwrap();
                match error_token_pair.as_rule() {
                    Rule::UNTERM_BADESC_STRLIT => writeln!(io::stderr(), "Unterminated string literal with bad escape sequence ignored"),
                    Rule::BADESC_STRLIT => writeln!(io::stderr(), "String literal with bad escape sequence ignored"),
                    Rule::UNTERM_STRLIT => writeln!(io::stderr(), "Unterminated string literal ignored"),
                    Rule::ILLEGAL_CHAR => writeln!(io::stderr(), "Illegal character {}", error_token_pair.as_str()),
                    _ => unreachable!(),
                }.unwrap();
            },
            Rule::SHORTLITERAL => {
                let tok = token_pair.as_str();
                let len = tok.len();
                let s = &tok[..len-1];
                let num: f64 = s.parse().unwrap();
                if num > SHORT_MAX {
                    let span = token_pair.as_span();
                    let (start_line, start_col) = span.start_pos().line_col();
                    let (end_line, end_col) = span.end_pos().line_col();
                    writeln!(io::stderr(), "FATAL [{},{}]-[{},{}]: Short literal overflow", start_line, start_col, end_line, end_col).unwrap();
                }
                write_token(&mut outfile, token_pair);
            },
            Rule::INTLITERAL => {
                let s = token_pair.as_str();
                let num: f64 = s.parse().unwrap();
                if num > INT_MAX {
                    let span = token_pair.as_span();
                    let (start_line, start_col) = span.start_pos().line_col();
                    let (end_line, end_col) = span.end_pos().line_col();
                    writeln!(io::stderr(), "FATAL [{},{}]-[{},{}]: Integer literal overflow", start_line, start_col, end_line, end_col).unwrap();
                }
                write_token(&mut outfile, token_pair);
            },
            _ => write_token(&mut outfile, token_pair), // treat everything else normally
        }
    }
}

fn write_token<'i, W: Write>(buf: &mut W, pair: pest::iterators::Pair<'i, Rule>) {
    let mut tokens = pair.tokens();
    let start_token = tokens.next().unwrap();
    let end_token = tokens.next().unwrap();
    let end_pos = match end_token {
        pest::Token::End{rule: _, pos} => pos,
        _ => unreachable!(),
    };
    if let pest::Token::Start{rule, pos: start_pos} = start_token {
        let (line, col) = start_pos.line_col();
        let slice = start_pos.span(&end_pos).as_str();
        let tokenspec = match rule {
            Rule::STRLITERAL => format!("STRINGLITERAL:{}", &slice),
            Rule::INTLITERAL => format!("INTLITERAL:{}", &slice),
            Rule::SHORTLITERAL => {
                let len = slice.len();
                format!("SHORTLITERAL:{}", &slice[..len-1])
            },
            Rule::ID => format!("ID:{}", &slice),
            Rule::EOI => String::from("EOF"),
            r => format!("{:?}", r),
        };
        match writeln!(buf, "{} [{},{}]", &tokenspec, &line, &col) {
            Ok(_) => (),
            Err(_) => {
                writeln!(io::stderr(), "Error writing token {} [{},{}] to output file", &tokenspec, &line, &col).unwrap(); 
            },
        }
    }
}
