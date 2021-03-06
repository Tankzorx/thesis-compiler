namespace Zorx


open Zorx.ASTHelpers
open Zorx.Frontend.AST
open Zorx.Frontend

module Typecheck =

    let loggingEnabled = true
    let logger msg = Zorx.Logging.logger loggingEnabled msg

    type TypeEnv = Dec list


    // Checks:
    // - No duplicate decls in dp status signals and controller decls.
    // - valid controller+dp
    let rec tcModule ((M (_, ctrl, dp))): bool =
        let (Controller (ctrlDecls, _)) = ctrl
        let (Datapath (dpDecls, _)) = dp

        // Ensure that there are no status signals that has the same name
        // as controller input/output
        let checkDuplicateDecls =
            List.filter (fun (Dec (_, t, _)) -> t = StatusSignal) dpDecls |>
            List.fold (fun acc (Dec (dpDecName, _, _)) ->
                let duplicateExists =
                    List.exists (
                        fun (Dec (ctrlDecName, _, _)) -> ctrlDecName = dpDecName) ctrlDecls
                if duplicateExists then 
                    logger (sprintf "Duplicate declaration found %A in controller/statussignals" dpDecName)
                acc &&
                (not duplicateExists)
            ) true

        checkDuplicateDecls &&
        tcController (ctrl, dp) &&
        tcDatapath (dp)

    // Checks:
    // - No status signal decls
    // - No reg decls
    // - No actions that aren't declared
    // - All transitions must pass type check
    and tcController (controller: Controller, dp: Datapath): bool =
        let (Controller (ctrlDecls, transitions)) = controller
        let (Datapath (_, actions)) = dp

        // There should be no status signal and reg decls.
        let checkStatusDecls =
            List.fold (fun acc dec -> 
                let (Dec (_, typ, _)) = dec
                if typ = StatusSignal || typ = Reg then
                    logger (sprintf "Variables of type '%A' is not allowed in the controller" typ)
                acc &&
                typ <> StatusSignal &&
                typ <> Reg
            ) true ctrlDecls

        // First fold to get a list of actionNames used in the transitions,
        // then fold to ensure that every used action is mentioned in the datapath.
        let actionsDeclared =
            List.fold (fun acc (T (_, _, actionNames, _, _) as t) ->
                acc &&
                List.fold (
                    fun acc actionName ->
                        let actionDeclared = List.exists (fun (Action (s, _)) -> s = actionName) actions
                        if not actionDeclared then
                            logger (sprintf "The action '%A' is used in %A but not declared in the datapath" actionName t)
                        acc && actionDeclared
                ) true actionNames
            ) true transitions

        if checkStatusDecls && actionsDeclared then
            List.fold (fun acc t -> acc && tcTransition (t, dp, controller)) true transitions
        else
            false

    // Checks:
    // - No variable is declared twice
    // - All actions must be single assignment
    // - All variables used must be declared
    and tcDatapath ((Datapath (decls, actions)): Datapath): bool =
        
        let declsDuplicatesRemoved =
            List.map (fun (Dec (n,_,_)) -> n) decls |> Set.ofList |> Set.toList

        let actionsWellFormed = 
            List.fold (fun acc action -> acc && (tcAction (action, decls))) true actions
        declsDuplicatesRemoved.Length = decls.Length &&
        actionsWellFormed

    and tcTransition (transition: Transition, dp: Datapath, controller: Controller): bool =
        // Checks:
        // - That the actions executed are single assignment
        // - That the guard is properly typed
        let (T (s, guard, actionNames, s', cStmts)) = transition
        let (Datapath (dpDecls, _)) = dp
        let (Controller (ctrlDecls, _)) = controller


        // The guard expression should be evaluated with all the controllers
        // variables, and the status signals of the dp.
        let transitionContext =
            ctrlDecls @
            List.filter (fun dec ->
                match dec with
                    | Dec (_, StatusSignal, _) -> true
                    | _ -> false
            ) dpDecls

        let tcGuardResult = tcExp (guard, transitionContext)
        if not tcGuardResult then
            logger (sprintf "Typecheck of '%A' in transtion '%A' failed." guard transition)

        let tcCStmts  = tcAction (Action ("", cStmts), transitionContext)

        // Reuse tcAction by constructing an auxilliary action
        // consisting of all statements from the actions to be
        // executed.
        let combinedStmL =
            List.fold (
                fun acc actionName ->
                    let (Action (_, stmL)) = getActionByName actionName dp
                    (stmL)@acc
            ) [] actionNames
        let tcActionResult = tcAction (Action ("", combinedStmL), dpDecls)
        if not tcActionResult then
            logger (sprintf "The actions of '%A [..](%A)> %A' is failing the typecheck." s actionNames s')

        tcActionResult && tcGuardResult && tcCStmts
        

    // Checks:
    // - Must be single assignment
    // - All statements must pass type check.
    // Annoying name collision with some built in type called Action
    and tcAction (action: AST.Action, decls: TypeEnv): bool =
        let (Action (_, stmL)) = action
        // The stmList must be single assignment
        if not (isSingleAssignment stmL) then
            logger (sprintf "The statement list '%A' assigns variables multiple times." stmL)
        isSingleAssignment stmL &&
        // Type check all statements individually
        List.fold (fun acc stm -> acc && (tcStm (stm, decls))) true stmL


    // Checks:
    // - Check that lHand is present in decls
    // - Check that type of statement is equal that of the expression.
    // - Check that output variables are not used in expressions.
    and tcStm (stm: Stm, decls: TypeEnv): bool =
        let (Ass (lHand, e)) = stm

        let lHandDeclared =
            List.exists (fun dec -> 
                let (Dec (name, _, _)) = dec
                name = lHand
            ) decls

        if not lHandDeclared then
            logger (sprintf "Variable %A is not declared in '%A'" lHand stm)
            false
        else
            let (Dec (_, typ, ptyp)) = getDecByName lHand decls
            let isInPort = typ = InPort
            if isInPort then
                logger (sprintf "Assigning InPort variables is not legal: %A" stm)
            let tcExpResult = tcExp (e, decls)
            if tcExpResult && not isInPort then
                let eptyp = typeOfExp (e, decls)
                if eptyp <> ptyp then
                    logger (sprintf "Type conflict: declared type %A not equal to actual type %A at %A" ptyp eptyp stm)
                eptyp = ptyp
            else 
                false

    // Checks:
    // - OutPorts can't be used in expressions.
    // - .. More obvious stuff.
    and tcExp (exp: Exp, decls: TypeEnv): bool =
        match exp with
            | C _ -> true
            | Access (AVar s) ->
                let isDeclared =
                    List.exists (fun dec -> 
                        let (Dec (varName, _, _)) = dec
                        varName = s
                    ) decls
                if not isDeclared then
                    logger (sprintf "Variable %A is not declared in %A" s decls)

                if isDeclared then
                    let (Dec (_, typ,_)) = 
                        List.find (fun dec -> 
                            let (Dec (varName, _, _)) = dec
                            varName = s
                        ) decls
                    let isOutPort = typ = OutPort
                    if isOutPort then
                        logger (sprintf "The variable %A, is an OutPor, and therefore can't be used in an expression." exp)
                    not isOutPort
                else
                    false
            | BExp (e1, op, e2) ->
                if tcExp (e1, decls) && tcExp(e2, decls) then
                    let ptyp1 = typeOfExp (e1, decls)
                    let ptyp2 = typeOfExp (e2, decls)
                    if ptyp1 <> ptyp2 then
                        logger (sprintf "ptyp1 <> ptyp2: %A" exp)
                    ptyp1 = ptyp2 &&
                    match op with
                        | Gt -> ptyp1 = Integer
                        | Lt -> ptyp1 = Integer
                        | Leq -> ptyp1 = Integer
                        | Geq -> ptyp1 = Integer
                        | Plus -> ptyp1 = Integer
                        | Minus -> ptyp1 = Integer
                        | And -> ptyp1 = PrimTyp.Boolean
                        | Or -> ptyp1 = PrimTyp.Boolean
                        | Neq -> true
                        | Eq -> true
                else
                    false
            | UExp (e, op) ->
                if tcExp (e, decls) then
                    let ptyp = typeOfExp (e, decls)
                    match op with
                        | Not -> ptyp = PrimTyp.Boolean
                else
                    false


    
