namespace Zorx


open Zorx.Frontend.AST

module ASTHelpers =

    type VarEnv = Map<string, Const>

    let isSingleAssignment (stmL: Stm list): bool =
        let lhOperands = List.map (
                            fun stm -> 
                            let (Ass (s, _)) = stm
                            s
                         ) stmL
        let filteredLHOperands = Set.ofList lhOperands |> Set.toList
        lhOperands.Length = filteredLHOperands.Length

    let getDecByName s decls: Dec =
        List.find (fun dec ->
            let (Dec(name, _, _)) = dec
            name = s
        ) decls

    let rec addVectorToEnv (input: (string * Const) list) (env: VarEnv) =
                match input with
                    | [] -> env
                    | (varName, c)::tl -> addVectorToEnv tl (env.Add(varName, c))


    let getActionByName actionName dp =
        let (Datapath (_, actionL)) = dp 
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


    let rec typeOfExp (exp: Exp, decls: Dec list): PrimTyp =
        match exp with
            | C (N _) -> Integer
            | C (B _) -> PrimTyp.Boolean
            | Access (AVar s) ->
                let (Dec (_, _, ptyp)) = getDecByName s decls
                ptyp
            | BExp (e1, op, _) ->
                let ptyp1 = typeOfExp (e1, decls)
                match op with
                    | Gt -> PrimTyp.Boolean
                    | Lt -> PrimTyp.Boolean
                    | Leq -> PrimTyp.Boolean
                    | Geq -> PrimTyp.Boolean
                    | Neq -> PrimTyp.Boolean
                    | Eq -> PrimTyp.Boolean
                    | Plus -> Integer
                    | Minus -> Integer
                    | And -> PrimTyp.Boolean
                    | Or -> PrimTyp.Boolean
            | UExp (_, op) ->
                match op with
                    | Not -> PrimTyp.Boolean

    // Only works for reasonable variables and values.
    // Long variables/huge values will not look pretty.
    let prettyPrintRun run =
        match List.tryHead run with
            | Some h -> 
                List.iter (fun (key, _) -> printf "%A   " key) h
                printfn ""
                List.iter (fun l ->
                    List.iter (fun item ->
                        let (_,value) = item
                        printf "|%A|\t" value
                    ) l
                    printf "\n"
                ) run
            | None -> ()
