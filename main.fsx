// open Zorx.Frontend.AST

#r @"bin/Debug/FSharp.PowerPack.dll";;

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "util.fs"
#load "ASTIntp.fs"

// open Zorx.Frontend.AST
open Zorx.Util
open Zorx.Interpreter
open Zorx.Frontend.AST

// parseFromFile "test/lexingTest.zorx"
let ast = parseFromFile "test/findLargest.zorx"
printfn "%A" ast
let (S ms) = ast
match ms with
    | [] -> failwith "Empty fsmd"
    | m::ms ->
        let (M (name, fsm, dp)) = m
        let (Fsm (decL, tL)) = fsm
        printfn "asd"
        match tL with
            | [] -> failwith "Empty transition list"
            | t::tls -> 
                printfn "30"
                let (T (s1, exp, actionList, s2)) = t
                printfn "31"
                let v = intpExp exp (Map.empty.Add("cIn", B true))
                printfn "33"
                printfn "%A" v
// intpExp 
// let dpInVector = Map.empty.Add("dpIn", 3)
// let findLargestFsmd: (string * Map<string, PrimTyp> -> int) = intpSpecification(ast)
//parseFromFile "test/datapathTest.zorx"
