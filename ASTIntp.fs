namespace Zorx


open Zorx.Frontend.AST

module Interpreter =

    type ControllerNextStateResult =
            | Success of string * string list * Stm list
            | Error of string

    let findTransitionsByStartState tList state =
        List.filter (fun t -> 
            let (T (stateName, _, _, _)) = t
            stateName = state
        ) tList

    let loggingEnabled = false
    let logger msg = Zorx.Logging.logger loggingEnabled msg

    type VarEnv = Map<string, Const>

    let initVarEnvFromDecls decls (ctx: VarEnv) =
        let rec innerF decls (ctx: VarEnv) =
            match decls with
                | [] -> ctx
                | dec::tail ->
                    match dec with
                        | Dec (varName, _, Boolean) ->
                            innerF tail (ctx.Add(varName, B false))
                        | Dec (varName, _, _) ->
                            innerF tail (ctx.Add(varName, N 0))
        innerF decls ctx


    let rec intpSpecification (ast: Specification) =
        let ( S modules) = ast
        match modules with
            | [] -> failwith "empty fsmd"
            | (x::_) ->
                // printfn "%A" x
                intpModule x
                // printfn "%s" s
    and intpModule (fsmdModule: Module) = 
        let (M (_, ctrl, dp)) = fsmdModule
        let (Controller (ctrlDecL, _)) = ctrl
        let (Datapath (dpDecL, _)) = dp

        // let actionNames = List.map (fun x -> x) actL
        let ctrlFunc = intpController ctrl
        let dpFunc = intpDp dp
        let startEnv = initVarEnvFromDecls dpDecL Map.empty |> initVarEnvFromDecls ctrlDecL
        let moduleFunc conf =
            let (state, ctx) = conf
            let (newState, actionList, _) = match ctrlFunc (state, ctx) with
                                            | Success (a, b, c) -> (a, b, c)
                                            | Error s -> failwith s
            let (newCtx) = dpFunc (actionList, ctx)
            (newState, newCtx)

        (moduleFunc, startEnv)
    
    and intpStm (stm: Stm, ctx: VarEnv)  =
        let (Ass (lval, exp)) = stm
        let value = intpExp exp ctx
        logger (sprintf "Assigning: %A := %A" lval value)
        (lval, value)

    // Should return nextState function. input: state, cIn, ss
    // Output: next state, control signal(action list), csOut
    and intpController (controller: Controller) =
        let (Controller (_, tL)) = controller
        let controllerFunc (state: string, (inputVector: Map<string, Const>)): ControllerNextStateResult =

            let validTransitions =
                findTransitionsByStartState tL state |>
                List.filter (fun t -> 
                    let (T (_, exp, _, _)) = t
                    let v = intpExp exp inputVector
                    v = B true
                ) 

            if validTransitions.Length = 0 then
                let errMsg = sprintf "No transition at: %A" state
                Error errMsg
            else
                if validTransitions.Length > 1 then
                    logger "Multiple possible transitions. Picking first"
                let (T (s1, exp, actions, s2)) = validTransitions.Head
                logger (sprintf "CTRL: transition %A->%A with %A" s1 s2 exp)
                Success (s2, actions, [])
        controllerFunc
    
    // Should return datapath function.
    // dp function has the following pseudo signature:
    // input: actionList, dpInput, ctx/registerState
    // output: status signals, new register state, dpOut
    and intpDp (dp: Datapath) =
        let (Datapath (_, actionL)) = dp
        let getActionByName actionName = 
            let rec inner actionLs =
                match actionLs with
                    | [] -> failwith (sprintf "No action matched the requested action: %A" actionName)
                    | a::tail ->
                        let (Action (name, _)) = a
                        if name = actionName then
                            a
                        else
                            inner tail
            inner actionL

        let dpFunc (actionList: string list, ctx: VarEnv) =
            logger (sprintf "DP: executing %A" actionList)
            logger "----Printing register state change -----"
            logger (sprintf "oldState: %A" ctx)

            let rec addVectorToEnv (input: (string * Const) list) (env: VarEnv) =
                match input with
                    | [] -> env
                    | (varName, c)::tl -> addVectorToEnv tl (env.Add(varName, c))
            // To ensure all actions are working on the same register state
            // (primed register state)
            // We push all changes to a list, and merge the changes later.
            let registerChanges =
                List.fold (fun acc actionName ->
                    let action = getActionByName actionName
                    (intpAction (action, ctx))@acc
                ) [] actionList

            logger (sprintf "registerChanges: %A" registerChanges)

            let newRegisterState = addVectorToEnv registerChanges ctx

            logger (sprintf "newState: %A" newRegisterState)
            logger "----/Printing register state change -----"

            newRegisterState

        dpFunc

    and intpAction (action: Action, ctx: VarEnv): (string * Const) list =
        let (Action (_, stmL)) = action
        List.fold (fun acc stm  ->
            let (varName, value) = intpStm (stm, ctx)
            (varName, value)::acc
        ) [] stmL

    and intpExp (exp: Exp) ctx: Const =
        match exp with
            | C (B value) -> B value
            | C (N value) -> N value
            | Access access    -> intpAccess access ctx
            | UExp (e1, op) ->
                let v1 = intpExp e1 ctx
                match v1, op with
                    | N _, Not -> failwith (sprintf "Operator 'Not', can't be applied to variable of type integer.")
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
                            | _  -> failwith (sprintf "Operator '%A' does not expect bool*bool, and got '%A'*'%A' " op v1 v2)
                    | N value1, N value2 ->
                        match op with
                            | Gt -> 
                                logger (sprintf "%A > %A" value1 value2)
                                B (value1 > value2)
                            | Eq -> B (value1 = value2)
                            | Neq -> B (value1 <> value2)
                            | Lt -> B (value1 < value2)
                            | Leq -> B (value1 <= value2)
                            | Geq -> B (value1 >= value2)
                            | Plus -> N (value1 + value2)
                            | Minus -> N (value1 - value2)
                            | _  -> failwith (sprintf "Operator '%A' does not expect int*int, and got: '%A'*'%A' " op v1 v2)

                    | _ -> failwith (sprintf "Operator: '%A' cannot be applied to '%A', '%A'" op v1 v2)

    and intpAccess (access: Access) (ctx: VarEnv) =
        let (AVar s) = access
        logger (sprintf "Accessing: %s (%A)" s ctx.[s])
        ctx.[s]

    let exec parsedModule startState (inputVector: (string * Const) list list) =
        let (transitionSystem, startEnv) = intpModule parsedModule

        let mapToList map = Map.fold (fun foldState key value -> ((key, value)::foldState)) [] map

        let rec addInputVectorToEnv input (env: VarEnv) =
            match input with
                | [] -> env
                | (varName, c)::tl -> addInputVectorToEnv tl (env.Add(varName, c))

        let rec inner state (env: VarEnv) inputVector (runVector: (string * Const) list list) =
            match inputVector with
                | [] -> List.rev runVector
                | cycleInput::rest ->
                    let env = addInputVectorToEnv cycleInput env
                    let (nextState, nextEnv) = transitionSystem (state, env)
                    let cycleOutput = mapToList nextEnv
                    logger "Cycle-----"
                    inner nextState nextEnv rest (cycleOutput::runVector)

        inner startState startEnv  inputVector [(mapToList startEnv)]
