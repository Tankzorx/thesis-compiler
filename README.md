# Compiler for FSMD

This repository contains the source code for a compiler, for a language built on a model named 'Finite State Machine with Datapath'.

A few programs in the language can be seen in the test folder.
In particular findLargest.zorx shold be an example that simply shows the language.
## Project Dependencies
FSharp/Mono should be installed on your system. Fslex and Fsyacc are both included in the repository.
The project was developed on a linux machine, and might need some work to run on windows.

## Generating Lexer and Parser
On linux, run yaccify.sh. This runs fslex and fsyacc, and adds the correct module name to the parser.
 (For some reason fsyacc did not want to add this by itself)

The repository contains generated parser and lexer of the current grammar.

## Running
Running main.fsx with an fsharp interpreter parses some of the test files, and tests them with input:
```
fsharpi main.fsx
```
## Development

`Lexer.fsl` contains the lexer specification. The tokens produced are however specified in `parser.fsy`.
`Parser.fsy` specifies the grammar. It produces an abstract syntax tree of the type defined in `AST.fs`.

A typechecker module is defined in `typecheck.fs`. The functions in this module is named `tc[Construct]`.
For example, to typecheck an expression, use `tcExp [params]`. To typecheck a module use `tcModule [params]`.

An interpreter module is defined in `intpAST.fs`. The functions for interpreting each construct is named `intp[Construct]`.
Some of these functions are of higher order, and returns a function that models the underlying construct. 
This concerns `intpModule`, `intpDp` and `intpController`. Use of this can be seen in `main.fsx`.

Each file has a logger defined, that can be enabled/disabled.
If work is done in a particular module, the logger of that module can be enabled to help debugging.
