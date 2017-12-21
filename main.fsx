
#r @"bin/Debug/FSharp.PowerPack.dll";;

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "util.fs"
#load "ASTIntp.fs"

// open Zorx.Frontend.AST
open Zorx.Util
open Zorx.Interpreter

// parseFromFile "test/lexingTest.zorx"
let ast = parseFromFile "test/findLargest.zorx"

let k = intpSpecification(ast)
//parseFromFile "test/datapathTest.zorx"
