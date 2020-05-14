open System
open Parser
open ProjectParser
open ProjectInterpreter


(*From course material*)
let parse input: Expr option =
    let input' = prepare input
    match grammar input' with
    | Success(res, _) -> Some res
    | Failure(pos, rule) ->
        printfn "Invalid expression."
        let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos input msg
        printf "%s" diag
        None

let printEval ast =
    try
        printfn "%A" (snd (eval [] ast))
    with ex -> printfn "Exception! %s " (ex.Message)

let parseAndEval input =
    let ast_opt =
        try
            (parse input)
        with ex ->
            printfn "Exception! %s " (ex.Message)
            None
    match ast_opt with
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

[<EntryPoint>]
let main argv =
    if not (Array.isEmpty argv) then parseAndEval (argv.[0]) else repl()
    0
