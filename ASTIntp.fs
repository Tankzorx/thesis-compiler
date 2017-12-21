namespace Zorx


open Zorx.Frontend.AST

module Interpreter =

    type Lul =
        | I of int 
        | B of string

    let intpSpecification (ast: Specification) =
        let ( S modules) = ast
        match modules with
            | [] -> (printfn "lul")
            | (x::xs) ->
                printfn "%A" x
                let (M (s, fsm, dp)) = x
                printfn "%s" s
