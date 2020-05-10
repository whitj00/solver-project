open System
open Parser
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    let input = prepare argv.[0]
    match grammar input with
    | Success(res, _) ->
        printfn "%A" (eval res)
        0
    | Failure _ ->
        printfn "Failure!"
        1
