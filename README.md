Ducky
=====

## Introduction

Ducky is a BASIC-like programming language originally insipred by Duckyscript, the USB Rubber Ducky's scripting language.

## Examples

### Hello World

    #!/bin/ducky
    LOG Hello, world!
    NEWLINE

Simple!

### Fibonacci Sequence

    #!/bin/ducky
    LET a = 0; LET b = 0; LET iter = 0
    LABEL loop_start
    LOGVAR a
    LET c = a+b
    LET a = b
    LET b = c
    INC iter
    IF iter < 20; GOTO loop_start
    LOG Done!

## Building

POSIX systems are supported, as are some Rockbox targets.

### Unix/Linux

    make
    sudo make install

This installs the `/bin/ducky` binary.

## Usage

### Running Directly (no compilation)

    ducky scriptname.ds

### Compiling to bytecode

    ducky -c scriptname.ds

This will create `a.out`, which contains the bytecode.

### Executing bytecode

    ducky a.out

## Future Directions

   - Compile to C?
   - Refactor code
   - Add more builtins
   - Arrays?
   - Functions?
