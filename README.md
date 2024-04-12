# SallyCompiler

An optimizing toy compiler written in C++, that translates a minimal C-like language (SysY) into ARMv7 assembly.

## Features

- Handmade Parser
- SSA form IR
- A handful of general optimization passes
- Graphviz based IR dumping

## Optimizations

### IR

- Global Value Numbering
- Global Code Motion
- Memory SSA
- Loop Unrolling
- Function Inlining
- SCCP
- ADCE
- Instruction Combining
- Simplify CFG
- Memory Copy Propagation
- Find and bake global constants into functions
- Use temp register when accumulating

### Assembly

- Graph coloring register allocation
- Optimization for division and multiplication by constant (e.g. a*2 becomes a << 1)
- Turn branches into conditional executions
- Merge basic blocks
- Remove redundant loads
- Remove identical moves

## Build & Usage

### On Windows (MSVC compiler required)

```batch
> compile.bat
> build\sally.exe -h
Usage: build\sally.exe [ -h | --help ] [ -O2 ] [ -o <output-asm = build\test.s> ] <input-file = code\first.sy>
```

### Compiling with Graphviz (Windows Only for now)

1. Install [Graphviz](http://www.graphviz.org/download/) to `C:\Program Files\Graphviz`
2. Make sure you have `Cambria Math` font installed, which usually comes with Word
3. Build the compiler using `compile.bat -cfg`
4. Compile your program using sally, and grab your IR diagram at `build\ir.pdf`

### On Linux:

```bash
$ ./compile.sh
$ build/sally -h
```

### Example on Raspberry Pi:

```bash
$ cat fib.sy
int fib(int n) {
    if (n <= 2) return 1;
    return fib(n-1)+fib(n-2);
}

int main() {
    putint(fib(44)); putch(10);
    return 0;
}
$ build/sally fib.sy -o fib.s
$ gcc fib.s misc/libsysy.a
$ ./a.out
701408733
```



## Warning

We drink three bottles of vodka before we write any code.
