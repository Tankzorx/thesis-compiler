namespace Zorx


open Zorx.Frontend.AST

module Typecheck =

    let isSingleAssignment (stmL: Stm list): bool =
        let LHOperands = List.map (
                            fun stm -> 
                            let (Ass (s, _)) = stm
                            s
                         ) stmL
        let filteredLHOperands = Set.ofList LHOperands |> Set.toList
        LHOperands.Length = filteredLHOperands.Length

    