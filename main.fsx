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

let ast = parseFromFile "test/findLargest.zorx"
printfn "%A" ast
let (S ms) = ast
match ms with
    | [] -> failwith "Empty fsmd"
    | m::ms ->
        let (M (name, fsm, dp)) = m
        let (Fsm (decL, tL)) = fsm
        match tL with
            | [] -> failwith "Empty transition list"
            | t::tls -> 
                let (T (s1, exp, actionList, s2)) = t
                let v = intpExp exp (Map.empty.Add("cIn", B true))
                printfn "%A" (v = (B true))
        let nextStateFunction = intpFsm fsm
        let dummyMap = Map.empty.Add("cIn", B false).Add("newLargestFound", B false)
        let nextStateRes = nextStateFunction ("Idle", dummyMap)
        
        match nextStateRes with
            | FsmNextStateResult.Success (state',_,_) ->
                printfn "%A" (state' = "Idle")
            | FsmNextStateResult.Error s -> printfn "false"

        let dpFunction = intpDp dp
        let dummyCtx = Map.empty.Add("dpIn", N 3)
        printfn "%A" dummyCtx
        let cycle1 = dpFunction (["Init"], dummyCtx)
        printfn "%A" cycle1
        let cycle1 = cycle1.Add("dpIn", N 4)
        let cycle2 = dpFunction (["ReadInput"], cycle1)
        printfn "%A" cycle2
        let cycle2 = cycle2.Add("dpIn", N 5)
        let cycle3 = dpFunction (["ReadInput"; "SetNewLargest"], cycle2)
        printfn "%A" cycle3
        

        
// intpExp 
// let dpInVector = Map.empty.Add("dpIn", 3)
// let findLargestFsmd: (string * Map<string, PrimTyp> -> int) = intpSpecification(ast)
//parseFromFile "test/datapathTest.zorx"
