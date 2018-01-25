namespace Zorx


open Zorx.ASTHelpers
open Zorx.Frontend.AST
open Zorx.Frontend

module Typecheck =

    let loggingEnabled = true
    let logger msg = Zorx.Logging.logger loggingEnabled msg

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

        // There should be no status signal decls.
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
            List.fold (fun acc (T (_, _, actionNames, _) as t) ->
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
    // - All actions must be single assignment
    // - All variables used must be declared
    and tcDatapath (datapath: Datapath): bool =
        let (Datapath (decls, actions)) = datapath
        
        let tcActionsResult = 
            List.fold (fun acc action -> acc && (tcAction (action, decls))) true actions

        tcActionsResult

    and tcTransition (transition: Transition, dp: Datapath, controller: Controller): bool =
        // Checks:
        // - That the actions executed are single assignment
        // - That the guard is properly typed
        let (T (s, guard, actionNames, s')) = transition
        let (Datapath (dpDecls, _)) = dp
        let (Controller (ctrlDecls, _)) = controller

        // The guard expression should be evaluated with all the controllers
        // variables, and the status signals of the dp.
        let guardContext =
            ctrlDecls @
            List.filter (fun dec ->
                match dec with
                    | Dec (_, StatusSignal, _) -> true
                    | _ -> false
            ) dpDecls
        let tcGuardResult = tcExp (guard, guardContext)
        if not tcGuardResult then
            logger (sprintf "Typecheck of '%A' in transtion '%A' failed." guard transition)


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

        tcActionResult && tcGuardResult
        

    // Checks:
    // - Must be single assignment
    // - All statements must pass type check.
    // Annoying name collision with some built in type called Action
    and tcAction (action: AST.Action, decls: Dec list): bool =
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
    and tcStm (stm: Stm, decls: Dec list): bool =
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
            let (Dec (_, _, ptyp)) = getDecByName lHand decls
            let tcExpResult = tcExp (e, decls)
            if tcExpResult then
                let eptyp = typeOfExp (e, decls)
                if eptyp <> ptyp then
                    logger (sprintf "Type conflict: declared type %A not equal to actual type %A at %A" ptyp eptyp stm)
                eptyp = ptyp
            else 
                false

    and tcExp (exp: Exp, decls: Dec list): bool =
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
                isDeclared
            | BExp (e1, op, e2) ->
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
            | UExp (e, op) ->
                let ptyp = typeOfExp (e, decls)
                match op with
                    | Not -> ptyp = PrimTyp.Boolean


    
