# rust-cmmc

A compiler written in Rust for the C-- programming language, invented by Drew Davidson
at KU for EECS 665 Compiler Construction.

## Usage

This project requires the Rust toolchain to build. Follow the instructions on https://www.rust-lang.org/
to install.

Running `make` will place a cmmc executable in the project root which conforms to
the project specs. However, I've also made convenience scripts which add cmmc
in your $PATH and add an option, `-e`, to compile a .cmm file directly to an
executable.

Here's how to use them:

```bash
$ make # (if you haven't already)
$ source env/setup.sh
$ cmmc -e /path/to/program.cmm -e /path/to/executable
```

The `-e` option is only available through the script env/bin/cmmc. This script is
added to your $PATH when you run `source env/setup.sh`.

Compiler Constructed 🐉 ⚔️