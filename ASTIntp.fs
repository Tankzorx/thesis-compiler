namespace Zorx


open Zorx.Frontend.AST
// open Zorx.Frontend.intpHelpers


module Interpreter =

    type FsmNextStateResult =
            | Success of string * string list * Exp
            | Error of string

    let findTransitionsByStartState tList state =
        List.filter (fun t -> 
            let (T (stateName, _, _, _)) = t
            stateName = state
        ) tList

    let initVarEnvFromDecls decls (ctx: Map<string, Const>) =
        let rec innerF decls (ctx: Map<string, Const>) =
            match decls with
                | [] -> ctx
                | dec::tail ->
                    match dec with
                        | RegDec (varName, _, Boolean) ->
                            innerF tail (ctx.Add(varName, B false))
                        | RegDec (varName, _, _) ->
                            innerF tail (ctx.Add(varName, N 0))
        innerF decls ctx

    
    let rec intpSpecification (ast: Specification) =
        let ( S modules) = ast
        match modules with
            | [] -> failwith "empty fsmd"
            | (x::xs) ->
                // printfn "%A" x
                intpModule x
                // printfn "%s" s
    and intpModule (fsmdModule: Module) = 
        let (M (s, fsm, dp)) = fsmdModule
        let (Fsm (fsmDecL, transL)) = fsm
        let (Datapath (dpDecL, actL)) = dp

        // let actionNames = List.map (fun x -> x) actL
        let fsmFunc = intpFsm fsm
        let dpFunc = intpDp dp
        let startEnv = initVarEnvFromDecls dpDecL Map.empty |> initVarEnvFromDecls fsmDecL
        let retVal conf =
            let (state, ctx) = conf
            let (newState, actionList, _) = match fsmFunc (state, ctx) with
                                            | Success (a, b, c) -> (a, b, c)
                                            | Error s -> failwith s
            let (newCtx) = dpFunc (actionList, ctx)
            (newState, newCtx)

        (retVal, startEnv)
    
    and intpStm (stm: Stm, ctx: Map<string, Const>)  =
        let (Ass (lval, exp)) = stm
        let value = intpExp exp ctx
        ctx.Add(lval, value)

    // Should return nextState function. input: state, cIn, ss
    // Output: next state, control signal(action list), csOut
    and intpFsm (fsm: Fsm) =
        let (Fsm (decL, tL)) = fsm

        let retVal (state: string, (inputVector: Map<string, Const>)): FsmNextStateResult =

            let validTransitions =
                findTransitionsByStartState tL state |>
                List.filter (fun t -> 
                    let (T (s1, exp, actions, s2)) = t
                    let v = intpExp exp inputVector
                    v = B true
                ) 

            if validTransitions.Length > 1 then
                let errMsg = (sprintf "Undeterministic automaton. Can pick between: %A" validTransitions)
                Error errMsg
            elif validTransitions.Length = 0 then
                let errMsg = sprintf "No transition at: %A" state
                Error errMsg
            else
                let (T (s1, exp, actions, s2)) = validTransitions.Head
                Success (s2, actions, C (B false))
        retVal
    
    // Should return datapath function.
    // dp function has the following pseudo signature:
    // input: actionList, dpInput, ctx/registerState
    // output: status signals, new register state, dpOut
    and intpDp (dp: Datapath) =
        let (Datapath (decL, actionL)) = dp
        let getActionByName actionName = 
            let rec inner actionLs =
                match actionLs with
                    | [] -> failwith (sprintf "No action matched the requested action: %A" actionName)
                    | a::tail ->
                        let (Action (name, stms)) = a
                        if name = actionName then
                            a
                        else
                            inner tail
            inner actionL

        let retVal (actionList: string list, ctx: Map<string, Const>) =
            let oldRegisterState = ctx
            List.fold (fun acc actionName ->
                // printfn "I'm folding, folding, folding.. %A" actionName
                let action = getActionByName actionName
                intpAction (action, oldRegisterState)
            ) ctx actionList

        retVal

    and intpAction (action: Action, ctx: Map<string, Const>): Map<string, Const> =
        let (Action (name, stmL)) = action
        List.fold (fun acc stm  -> intpStm (stm, acc)) ctx stmL

    and intpExp (exp: Exp) ctx: Const =
        // failwith "NYI"
        match exp with
            | C (B value) -> B value
            | C (N value) -> N value
            | Access access    -> intpAccess access ctx
            | UExp (e1, op) ->
                let v1 = intpExp e1 ctx
                match v1, op with
                    | N value1, Not -> failwith (sprintf "Operator 'Not', can't be applied to variable of type integer.")
                    | B value1, Not -> B (not value1)
            | BExp (e1, op, e2) ->
                let v1 = intpExp e1 ctx
                let v2 = intpExp e2 ctx
                match v1, v2 with
                    | B value1, B value2 -> 
                        match op with
                            | Eq -> B (value1 = value2)
                            | Neq -> B (value1 <> value2)
                            | And -> B (value1 && value2)
                            | Or -> B (value1 || value2)
                            // | Eq -> B (value1 = value2)
                            | _  -> failwith (sprintf "Operator '%A' expects bool*bool, but got '%A'*'%A' " op v1 v2)
                    // | And -> B (B v1 && B v2)
                    // | Plus -> N (v1 + v2)
                    | N value1, N value2 ->
                        match op with
                            | Gt -> B (value1 > value2)
                            | Eq -> B (value1 = value2)
                            | Neq -> B (value1 <> value2)
                            | Lt -> B (value1 < value2)
                            | Leq -> B (value1 <= value2)
                            | Geq -> B (value1 >= value2)
                            | Plus -> N (value1 + value2)
                            | Minus -> N (value1 - value2)
                            | _  -> failwith (sprintf "Operator '%A' expects int*int, but got '%A'*'%A' " op v1 v2)

                    | _ -> failwith (sprintf "Operator: '%A' cannot be applied to '%A', '%A'" op v1 v2)
            | _ -> failwith (sprintf "NYI interpret for: '%A'" exp)

    and intpAccess (access: Access) (ctx: Map<string, Const>) =
        let (AVar s) = access
        printfn "Accessing: %s" s
        ctx.[s]


