
#r @"bin/Debug/FSharp.PowerPack.dll";;

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "util.fs"

// open Zorx.Frontend.AST
open Zorx.Util

//parseFromFile "test/lexingTest.zorx"
parseFromFile "test/lexingTest.zorx"
//parseFromFile "test/datapathTest.zorx"
