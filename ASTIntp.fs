namespace Zorx


open Zorx.Frontend.AST
// open Zorx.Frontend.intpHelpers


module Interpreter =

    let findTransitionsByStartState tList state =
        List.filter (fun t -> 
            let (T (stateName, _, _, _)) = t
            stateName = state
        ) tList
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
        printfn "doneski"
        fsmFunc
    
    // Should return nextState function. input: state, cIn, ss
    // Output: next state, control signal(action list), csOut
    and intpFsm (fsm: Fsm) =
        let (Fsm (decL, tL)) = fsm
        let retVal (state: string, (inputVector: Map<string, Const>)) =
            // possible by start state
            let possibleTransitions = findTransitionsByStartState tL state
            let transitions = 
                List.filter (fun t -> 
                    let (T (s1, exp, actions, s2)) = t
                    let v = intpExp exp inputVector
                    v = B true
                ) possibleTransitions
            failwith "NYI"
        // failwith "NYI"
        retVal
                
    
    and intpExp (exp: Exp) ctx: Const =
        // failwith "NYI"
        match exp with
            | C (B value) -> B value
            | C (N value) -> N value
            | Access access    -> intpAccess access ctx
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
        printfn "sddsdad"
        let (AVar s) = access
        ctx.[s]


