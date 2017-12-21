namespace Zorx
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

module Util =
    open Parser
    // open Microsoft.FSharp
    let parseString (text:string) =
        let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
        try
            let k = start Lexer.tokenize lexbuf
            // printfn "%A" k
            k
        with e ->
            let pos = lexbuf.EndPos
            //  printfn "Error near line %d, character %d\n" pos.Line pos.Column
            //  printfn
            failwith (sprintf "Parse failure at line %d col %d, at token '%s'" (pos.Line+1) pos.Column (System.Text.Encoding.ASCII.GetString(lexbuf.Lexeme)))

    let parseFromFile filename =
        if File.Exists(filename)
        then parseString(File.ReadAllText(filename))
        else invalidArg "ParserUtil" "File not found" 
