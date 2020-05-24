open System.IO
open Parser
open ProjectParser
open ProjectInterpreter

let printEval ast =
    try
        eval Map.empty ast |> ignore
    with ex -> printfn "Exception! %s" (ex.Message)

let parseAndEval input =
    let astOpt =
        try
            (parse input)
        with ex ->
            printfn "Exception! %s " (ex.Message)
            None
    match astOpt with
    | Some ast -> (printEval ast)
    | None -> ()

(*Derived course material*)
let rec repl() =
    printf "Enter an expression (or 'quit' to quit): "
    let input = System.Console.ReadLine()
    if input = "quit" then
        printfn "Goodbye!"
        exit 0
    else
        parseAndEval input
        repl()

let readFile file =
    let file = String.concat " " (File.ReadAllLines(file))
    parseAndEval file

[<EntryPoint>]
let main argv =
    if not (Array.isEmpty argv) then readFile (argv.[0]) else repl()
    0
