namespace Zorx


open Zorx.Frontend.AST
open System

module Typecheck =

    type TcResult = 
        | Success
        | Error of string
    let logging = true
    let logger msg =
        if logging then
            printfn "%A" msg
        else
            ()

    let isSingleAssignment (stmL: Stm list): bool =
        let LHOperands = List.map (
                            fun stm -> 
                            let (Ass (s, _)) = stm
                            s
                         ) stmL
        let filteredLHOperands = Set.ofList LHOperands |> Set.toList
        LHOperands.Length = filteredLHOperands.Length

    let getDecByName s decls: Dec =
        List.find (fun dec ->
            let (RegDec(name, t, ptyp)) = dec
            name = s
        ) decls

    let getActionByName actionName dp =
        let (Datapath (_, actionL)) = dp 
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

    let rec typeOfExp (exp: Exp, decls: Dec list): PrimTyp =
        match exp with
            | C (N _) -> Integer
            | C (B _) -> PrimTyp.Boolean
            | Access (AVar s) ->
                let (RegDec (name, t, ptyp)) = getDecByName s decls
                ptyp
            | BExp (e1, op, e2) ->
                let ptyp1 = typeOfExp (e1, decls)
                let ptyp2 = typeOfExp (e2, decls)
                match op with
                    | Gt -> PrimTyp.Boolean
                    | Lt -> PrimTyp.Boolean
                    | Leq -> PrimTyp.Boolean
                    | Geq -> PrimTyp.Boolean
                    | Eq -> ptyp1
                    | Plus -> Integer
                    | Minus -> Integer
                    | And -> PrimTyp.Boolean
                    | Or -> PrimTyp.Boolean
            | UExp (e1, op) ->
                match op with
                    | Not -> PrimTyp.Boolean

    // Checks:
    // - No status signal decls
    // - No reg decls
    // - No actions that aren't declared?
    // - Requested actions must be single assignment programs
    // - All variables used must be declared
    let rec tcController (controller: Controller, dp: Datapath): bool =
        failwith "NYI: tcController"

    and tcDatapath (datapath: Datapath): bool =
        // Checks:
        // - All actions must be single assignment
        // - All variables used must be declared
        failwith "NYI: tcDatapath"

    and tcTransition (transition: Transition, dp: Datapath): bool =
        // Checks:
        // - That the actions executed are single assignment
        // -
        let (T (s, guard, actionNames, s')) = transition
        // let combinedStmL = 
        failwith "NYI: tcTransition"

    // Annoying name collision with some built in type called Action
    and tcAction (action: Zorx.Frontend.AST.Action, decls: Dec list): bool =
        let (Action (name, stmL)) = action
        // The stmList must be single assignment
        isSingleAssignment stmL &&
        // Type check all statements individually
        List.fold (fun acc stm -> acc && (tcStm (stm, decls))) true stmL


    and tcStm (stm: Stm, decls: Dec list): bool =
        // Checks:
        //  - Check that type of statement is equal that of the expression.
        let (Ass (lHand, e)) = stm
        let (RegDec (_, _, ptyp)) = getDecByName lHand decls
        let eptyp = typeOfExp (e, decls)
        if eptyp <> ptyp then
            logger (sprintf "Type conflict: declared type %A not equal to actual %A at %A" ptyp eptyp stm)
        eptyp = ptyp

    and tcExp (exp: Exp, decls: Dec list): bool =
        match exp with
            | C _ -> true
            | Access _ -> true
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


    
