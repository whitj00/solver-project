open System
open Parser
open ProjectParser
open ProjectInterpreter

(*From course material*)
let rec repl() =
    printf "Enter an expression (or 'quit' to quit): "
    let input = System.Console.ReadLine()
    if input = "quit" then
        printfn "Goodbye!"
        exit 0
    else
        let ast_opt = parse input
        match ast_opt with
        | Some ast -> printfn "%A" (eval ast)
        | None     -> ()
        repl()

[<EntryPoint>]
let main argv =
    repl()
    0
