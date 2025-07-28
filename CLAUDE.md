# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About Caescript

Caescript is a dialect of the Monkey programming language written in Rust. It features both interpreted and compiled implementations with a tree-walking evaluator and a stack-based virtual machine.

## Build Commands

The project uses Cargo with Make for common tasks:

- **Build VM version (recommended)**: `make build` or `cargo build --release --bin caescript --features=build-binary,vm`
- **Build interpreter version**: `make build_eval` or `cargo build --release --bin caescript --features=build-binary`
- **Install VM version**: `make install`
- **Install interpreter version**: `make install_eval`
- **Run tests**: `make test` or `cargo test --all-features`

## Running the Language

- **REPL**: `caescript`
- **Run file with VM**: `caescript vm examples/hello.cae`
- **Run file with interpreter**: `caescript eval examples/hello.cae`

## Architecture Overview

The codebase is organized into several key modules:

### Core Components

- **`src/lexer/`**: Tokenizes source code into tokens
- **`src/parser/`**: Builds AST from tokens using recursive descent parsing
- **`src/ast/`**: Defines Abstract Syntax Tree node types and structures
- **`src/token/`**: Token definitions and types

### Execution Engines

The project implements two execution models:

1. **Tree-walking Evaluator** (`src/eval/`):
   - Direct AST interpretation
   - Environment-based variable scoping
   - Object system with built-in functions
   - Enabled by default

2. **Bytecode Compiler + VM** (`src/compiler/` + `src/vm/`):
   - Compiles AST to bytecode instructions
   - Stack-based virtual machine execution
   - Symbol table for variable resolution
   - Requires `vm` feature flag

### Key Architecture Points

- **Feature flags**: The VM components are gated behind the `vm` feature
- **Binary target**: Main executable requires `build-binary` feature for REPL functionality
- **Dual execution**: Same AST can be evaluated directly or compiled to bytecode
- **Object system**: Shared between evaluator and VM for runtime values
- **Scoping**: Compiler uses symbol tables, evaluator uses environment chains

### Module Dependencies

- `ast` → Core AST definitions used by parser, compiler, and evaluator
- `parser` → Uses `lexer` and `token`, produces `ast`
- `compiler` → Takes `ast`, produces bytecode for `vm`
- `eval` → Takes `ast`, executes directly with environment management
- `vm` → Executes compiled bytecode with stack-based execution model

The architecture supports both educational exploration (tree-walking) and performance (bytecode VM) while sharing the same front-end parsing infrastructure.