// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use std::io::{self, Write};
use crate::ast::Pos;

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
