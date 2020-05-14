module MiniMax

open ProjectParser

let childGen children =
    if List.isEmpty children then None else Some children

type MaxTree(board: Expr, children: MinTree list, value: int) =
    member this.Board = board
    member this.Children = childGen children
    member this.Player = Player(1)
    member this.Value = value

and MinTree(board: Expr, children: MaxTree list, value: int) =
    member this.Board = board
    member this.Children = childGen children
    member this.Player = Player(2)
    member this.Value = value

let validMoves (initBoard: 'a): 'b list = failwith "no implementation"

let minInt = -2147483648
let maxInt = 2147483647

let checkWin (board: Expr) state: Expr = failwith "No implementation"

let minValue (children: MaxTree list option): int =
    match children with
    | Some l ->
        List.fold (fun acc (x: MaxTree) ->
            if x.Value < acc then x.Value else acc) maxInt l
    | None -> minInt

let maxValue (children: MinTree list option): int =
    match children with
    | Some l ->
        List.fold (fun acc (x: MinTree) ->
            if x.Value > acc then x.Value else acc) minInt l
    | None -> maxInt

let rec genMaxTree (board: Expr) =
    let children = List.map genMinTree (validMoves board)
    MaxTree(board, children, maxValue (childGen children))

and genMinTree (board: Expr) =
    let children = List.map genMaxTree (validMoves board)
    MinTree(board, children, minValue (childGen children))
