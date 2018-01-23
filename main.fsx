// open Zorx.Frontend.AST

#r @"bin/Debug/FSharp.PowerPack.dll";;

#load "AST.fs"
#load "ASTHelpers.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "util.fs"
#load "ASTIntp.fs"
#load "typecheck.fs"

// open Zorx.Frontend.AST
open Zorx.Util
open Zorx.Interpreter
open Zorx.Typecheck
open Zorx.Frontend.AST
open Zorx.ASTHelpers


let ast = parseFromFile "test/findLargest.zorx"
printfn "%A" ast
let (S ms) = ast
match ms with
    | [] -> failwith "Empty fsmd"
    | m::ms ->
        let (M (name, fsm, dp)) = m
        // let (Fsm (decL, tL)) = fsm
        // match tL with
        //     | [] -> failwith "Empty transition list"
        //     | t::tls -> 
        //         let (T (s1, exp, actionList, s2)) = t
        //         let v = intpExp exp (Map.empty.Add("cIn", B true))
        //         printfn "%A" (v = (B true))
        // let nextStateFunction = intpFsm fsm
        // let dummyMap = Map.empty.Add("cIn", B false).Add("newLargestFound", B false)
        // let nextStateRes = nextStateFunction ("Idle", dummyMap)
        
        // match nextStateRes with
        //     | FsmNextStateResult.Success (state',_,_) ->
        //         printfn "%A" (state' = "Idle")
        //     | FsmNextStateResult.Error s -> printfn "false"

        // let dpFunction = intpDp dp
        // let dummyCtx = Map.empty.Add("dpIn", N 3)
        // printfn "%A" dummyCtx
        // let cycle1 = dpFunction (["Init"], dummyCtx)
        // printfn "%A" cycle1
        // let cycle1 = cycle1.Add("dpIn", N 4)
        // let cycle2 = dpFunction (["ReadInput"], cycle1)
        // printfn "%A" cycle2
        // let cycle2 = cycle2.Add("dpIn", N 5)
        // let cycle3 = dpFunction (["ReadInput"; "SetNewLargest"], cycle2)
        // printfn "%A" cycle3
        // Individual components seems to be working.
        // Now need to set up the complete transition system.
        // let (fsmFunc, dpFunc, startEnv) = intpModule m
        // let (transitionSystem, startEnv) = intpModule m
        // let startEnv = startEnv.Add("cIn", B true).Add("dpIn", N 3)
        // printfn "%A" startEnv
        // let conf0 = ("Idle", startEnv)
        // let (s1, ctx1) = transitionSystem conf0
        // printfn "%A, %A" s1 ctx1
        // let (s2, ctx2) = transitionSystem (s1, ctx1.Add("dpIn", N 4))
        // printfn "%A, %A" s2 ctx2
        // let (s3, ctx3) = transitionSystem (s2, ctx2.Add("dpIn", N 5).Add("cIn", B false))
        // printfn "%A, %A \n\n" s3 ctx3



        // Testing runner function.
        let inputVector = [
            [("cIn", B true); ("dpIn", N 3)];
            [("cIn", B true); ("dpIn", N 5)];
            [("cIn", B true); ("dpIn", N 4)];
            [("cIn", B false); ("dpIn", N 5)];
            // [("cIn", B false); ("dpIn", N 3)];
            // [("cIn", B false); ("dpIn", N 3)];
        ]
        let testRun = exec m "Idle" inputVector

        List.map (fun l -> printfn "%A\n" l) testRun |> ignore
        
        // Expr test
        let ctx = Map.empty.Add("x", N 4)
        let exp1 = C (N 4)
        let exp2 = Access (AVar "x")
        printfn "%A" (intpExp (BExp (exp1, Plus, exp2)) ctx)


        // Action test
        let ctx = Map.empty.Add("x", N 1).Add("y", B false)
        let stm1 = Ass ("x", C (N 5))
        let stm2 = Ass ("y", C (B true))
        let stmList = [stm1; stm2]
        let action = Action ("act1", stmList)
        printfn "%A" (intpAction (action, ctx))
        
        // Dp test
        // Construct some declarations
        let dec1 = RegDec ("a", Reg, Integer)
        let dec2 = RegDec ("b", InPort, Boolean)
        let dec3 = RegDec ("c", OutPort, Integer)
        let decls = [dec1; dec2; dec3]

        // Construct some actions
        let stm1 = Ass ("b", C (B false))
        let stm2 = Ass ("a", C (N 6))
        let stm3 = Ass ("c", BExp (C (N 4), Plus, (C (N 5))))
        let act1 = Action ("act1", [stm1; stm2])
        let act2 = Action ("act2", [stm3; stm2])

        // Construct dp, and create dpFunc
        let dp = Datapath (decls, [act1; act2])
        let dpFunc = intpDp dp

        // Construct context
        let ctx = Map.empty.Add("a", N 1).Add("b", B true).Add("c", N 5)
        printfn "%A" (dpFunc (["act1"; "act2"], ctx))



        // TYPECHECKING
        printfn "%A" (typeOfExp (C (N 4), []))

        let dec1 = RegDec ("a", Reg, Integer)
        let dec2 = RegDec ("b", InPort, Boolean)
        let dec3 = RegDec ("c", OutPort, Integer)
        let decls = [dec1; dec2; dec3]

        let e1 = C (N 4)
        let e2 = Access (AVar "a")
        let e3 = BExp (e1, Plus, e2)

        let stm1 = Ass ("a", C (N 6))
        let stm2 = Ass ("b", C (B false))
        let stm3 = Ass ("c", BExp (C (N 4), Plus, (C (N 5))))

        printfn "%A" (tcExp (e3, decls))
        printfn "%A" (tcStm (stm1, decls))

        let act1 = Action ("act1", [stm1; stm2])
        let act2 = Action ("act2", [stm3; stm2])

        // printfn "%A" (tcAction act1)

        let (S (mlist)) = parseFromFile "test/typecheckTransition.zorx"
        printfn "%A" mlist
        let (M (_, controller, dp)) = mlist.Head
        let (Controller (decls, transitions)) = controller

        // List.map (fun t -> tcTransition (t, dp, controller)) transitions |> ignore


        printfn "dp tc: %A" (tcDatapath dp)
        printfn "ctrl tc: %A" (tcController (controller, dp))


