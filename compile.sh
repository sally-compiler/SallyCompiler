#!/bin/sh

set -eu

CC=${CC:-g++}

mkdir -p build
# ${CC} $* -g -O0 -o build/sally code/sally.cpp
${CC} $* -g -O0 -o build/sally code/sally.cpp code/arm.cpp code/ast.cpp code/general.cpp code/parser.cpp code/tokenizer.cpp code/asm/asm_passes.cpp code/opt/opt.cpp code/ir.cpp
