open System
open Parser
open ProjectParser
open ProjectInterpreter

let printEval ast =
    try printfn "%A" (eval ast)
    with ex -> printfn "Exception! %s " (ex.Message)

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
        | Some ast -> (printEval ast)
        | None -> ()
        repl()

[<EntryPoint>]
let main argv =
    //printfn "%A" (List.collect (evalAllChanges (Num 0) (Num 1)) (List.collect (evalAllChanges (Num 0) (Num 2)) (List.collect (evalAllChanges (Num 0) (Num 1)) (List.collect (evalAllChanges (Num 0) (Num 2)) (List.collect (evalAllChanges (Num 0) (Num 1)) (List.collect (evalAllChanges (Num 0) (Num 2)) (List.collect (evalAllChanges (Num 0) (Num 1)) (List.collect (evalAllChanges (Num 0) (Num 2)) (evalAllChanges (Num 0) (Num 1) (List([Num 0; Num 0; Num 0; Num 0; Num 0; Num 0; Num 0; Num 0; Num 0])))))))))))
    repl()
    0
