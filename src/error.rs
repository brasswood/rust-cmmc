use std::io::{self, Write};
use crate::ast::Pos;
use crate::peg::Rule;

pub fn error(pos: &Pos, message: &str) {
    let Pos { start: (line1, col1), end: (line2, col2) } = pos;
    writeln!(
        io::stderr(),
        "FATAL [{},{}]-[{},{}]: {}",
        line1,
        col1,
        line2,
        col2,
        message
    ).unwrap();
}
