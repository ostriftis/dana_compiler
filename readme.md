# Dana Compiler

Compiler for the academic programming language **Dana**, described in the file *dana.pdf.


## Authors
 
- **Striftis Georgios** - 03121200
- **Papadopoulos Charalampos** - 03120199

---

## Technical Implementation
The dc compiler is implemented using a classic multi-pass architecture, leveraging industry-standard tools for high performance and maintainability.

Compiler Pipeline:
- Lexer (Flex): Performs lexical analysis, converting raw source code into a stream of tokens.
- Parser (Bison): Performs syntax analysis using an LALR(1) grammar, constructing an Abstract Syntax Tree (AST).
- Semantic Analyzer: A custom C++ pass that performs type checking, scope resolution, and symbol table management to ensure the Dana program is logically sound.
- Backend (LLVM): The AST is traversed to generate LLVM IR. This allows the compiler to benefit from LLVM’s powerful optimization passes and cross-platform machine code generation.

---



## Build Instructions

Build the compiler from within the root directory:

```bash
make
```

The compiler executable `dc` will be created in the same directory.

The source code is located within the src/llvm directory.

---

## Dependencies

Tested on **Ubuntu 24.04** with the following tools:

- **LLVM**: version 18.1
- **Clang**: version 18.1.3
- **Bison**: GNU Bison 3.8.2
- **Flex**: version 2.6.4
- **Library:** to support dana commands. Located within the src/libs directory

To install dependencies:

```bash
sudo apt update
sudo apt install -y llvm-18 clang-18 flex bison
```

## Command-Line Options

```
Usage: dc [options] [input_file]

Options:
  -i              Output intermediate code (LLVM IR) to stdout
  -f              Output final code (Assembly) to stdout
  -O              Enable optimizations
  -o <file>       Output executable to <file> (default: a.out)
  --help          Show this help message

If no -i or -f flags are given:
  - Intermediate code (.imm) and assembly (.asm) files are created
  - Files are placed in the same directory as the input file
  - Executable is created in the current directory
```

### Examples

```bash
# Normal compilation - creates .imm, .asm, and executable
dc program.dana

# View LLVM IR
dc -i < program.dana

# View Assembly code
dc -f < program.dana

# Compile with optimizations
dc -O program.dana

# Custom output name
dc -o myapp program.dana

# Combined flags
dc -O -o optimized_app program.dana
```

---

## Automated Testing

Switch to the `examples` folder:

```bash
cd examples
```

### Batch Compilation Scripts

**Compile all Dana programs**:

```bash
./compile_all.sh        # Includes optimization
```

**Compile only known working programs**:

```bash
./compile_working.sh    # Includes optimization
```

**Test error handling (19 erroneous programs)**:

```bash
./compile_erroneous.sh
```

### Testing Compiled Programs

Run automated input/output tests:

```bash
./testing.sh
```

This will:

- Test all executables in `executables/` directory
- Compare outputs with expected results
