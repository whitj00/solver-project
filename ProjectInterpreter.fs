module rec ProjectInterpreter

open ProjectParser

(* HELPER FUNCTIONS *)
let getNum n =
    match n with
    | Num n' -> n'
    | _ -> failwith ((prettyPrint n) + " is not a number")

let getNums ns = List.map getNum ns

let getList l =
    match l with
    | List l' -> l'
    | _ -> failwith ((prettyPrint l) + " is not a list")

let truthy (v: Expr): bool =
    match v with
    | Bool b -> b
    | Num n -> n > 0
    | _ -> failwith ("Value " + prettyPrint v + " is not truthy/falsy")

(* Boolean Evaluators *)
let rec evalAnd (al: Expr list): bool =
    if List.isEmpty al then
        true
    else
        match truthy (List.head al) with
        | true -> evalAnd (List.tail al)
        | false -> false

let rec evalOr (ol: Expr list): bool =
    if List.isEmpty ol then
        false
    else
        match truthy (List.head ol) with
        | true -> true
        | false -> evalOr (List.tail ol)

let evalNot op: bool = not (truthy op)

let evalIf (i, t, e): Expr =
    match truthy i with
    | true -> t
    | false -> e

(* Comparison Evaluators *)

let rec evalEq nums =
    match List.length nums with
    | 0 -> failwith "Eq takes at least one argument"
    | 1 -> true
    | _ -> (nums.[0] = nums.[1]) && evalEq (List.tail nums)

let rec evalCompare (nums: int list) op: bool =
    match List.length nums with
    | 0
    | 1 -> true
    | _ -> (op nums.[0] nums.[1]) && (evalCompare (List.tail nums) op)

let evalLt nums = evalCompare nums (<)
let evalGt nums = evalCompare nums (>)
let evalLte nums = evalCompare nums (<=)
let evalGte nums = evalCompare nums (>=)

(* Math Evaluators *)

let rec evalMultiply nums =
    match List.length nums with
    | 0 -> 1
    | _ -> nums.[0] * (evalMultiply (List.tail nums))

let rec evalAdd nums =
    match List.length nums with
    | 0 -> 0
    | _ -> nums.[0] + (evalAdd (List.tail nums))

let rec evalDivide (nums: int list) =
    match List.length nums with
    | 0 -> 1
    | _ -> List.head nums / evalMultiply (List.tail nums)

let rec evalSubtract nums =
    match List.length nums with
    | 0 -> 0
    | _ -> nums.[0] - (evalAdd (List.tail nums))

let evalMath op nums =
    match op with
    | "*" -> Num(evalMultiply (getNums nums))
    | "+" -> Num(evalAdd (getNums nums))
    | "/" -> Num(evalDivide (getNums nums))
    | "-" -> Num(evalSubtract (getNums nums))
    | "<" -> Bool(evalLt (getNums nums))
    | ">" -> Bool(evalGt (getNums nums))
    | "<=" -> Bool(evalLte (getNums nums))
    | ">=" -> Bool(evalGte (getNums nums))
    | "=" -> Bool(evalEq nums)
    | _ -> failwith "Invalid Operation"

(* Application Evaluators*)
let evalApp a: Expr =
    match List.head a with
    | Operation o -> evalMath o (List.tail a)
    | _ -> failwith "Invalid Function Name"

(* List Evaluators *)
let evalVal index board =
    match index < List.length board with
    | true -> board.[index]
    | false -> failwith ("Index " + string index + " out-of-bounds")

let evalLen board = List.length (getList board)

let evalAllChanges init change list =
    let rec changeHelper (prev: Expr list) (curr: Expr) next acc =
        match (prev, curr, next) with
        | (_, x, []) when x = init -> (List(prev @ [ change ])) :: acc
        | (_, x, _) when x = init ->
            changeHelper (prev @ [ curr ]) next.Head next.Tail (List(prev @ (change :: next)) :: acc)
        | (_, _, []) -> acc
        | (_, _, _) -> changeHelper (prev @ [ curr ]) next.Head next.Tail acc

    let convertedList = getList list
    changeHelper [] convertedList.Head convertedList.Tail []

let evalAppend l = List.concat l

(* Variable Eval *)
let evalVar (state: Map<string, Expr>) name =
    if state.ContainsKey name
    then state.[name]
    else failwith ("Variable " + name + " is not defined.")


let evalBoardDef (state: Map<string, Expr>) (expr: Expr) = (state.Add("board", expr), NoRet)


let evalWinDef player (state: Map<string, Expr>) expr =
    match player with
    | Player(1) -> (state.Add("p1Wins", expr), NoRet)
    | Player(2) -> (state.Add("p2Wins", expr), NoRet)
    | Player(0) -> (state.Add("_neitherWin", expr), NoRet)
    | _ -> failwith "Invalid player passed as argument."

let evalMoveDef player (state: Map<string, Expr>) expr =
    match player with
    | Player(1) -> (state.Add("p1Moves", expr), NoRet)
    | Player(2) -> (state.Add("p2Moves", expr), NoRet)
    | _ -> failwith "Invalid player passed as argument."


let rec evalProgram state expr exprAcc: Expr list =
    if List.isEmpty expr then
        exprAcc
    else
        let (newState, ret) = eval state (List.head expr)
        match ret with
        | NoRet -> ()
        | e -> printfn "%s" (prettyPrint e)
        evalProgram newState (List.tail expr) (exprAcc @ [ ret ])

let eval (state: Map<string, Expr>) (otherParam: Expr) =
    let getValue expr = snd (eval state expr)
    match otherParam with
    | Num n -> state, Num n
    | Bool b -> state, Bool b
    | Player n -> state, Player n
    | SavedApp f -> state, SavedApp f
    | Variable s -> state, evalVar state s
    | Application(ex, el) -> state, evalApp (getValue ex :: List.map getValue el)
    | AndOp al -> state, Bool(evalAnd (List.map getValue al))
    | OrOp ol -> state, Bool(evalOr (List.map getValue ol))
    | IfOp(i, t, e) -> eval state (evalIf (getValue i, getValue t, getValue e))
    | NotOp o -> state, Bool(evalNot (getValue o))
    | LenOp l -> state, Num(evalLen (getValue l))
    | Program p -> state, Program(evalProgram state p [])
    | List l -> state, List(List.map getValue l)
    | Operation o -> state, Operation o
    | ValOp(i, b) -> state, evalVal (getNum (getValue i)) (getList (getValue b))
    | WinDefOp(p, sf) -> evalWinDef p state (getValue sf)
    | ChangeOp(i, c, b) -> state, List(evalAllChanges (getValue i) (getValue c) (getValue b))
    | AppendOp l -> state, List(evalAppend (List.map (getValue >> getList) l))
    | BoardDefOp d -> evalBoardDef state (getValue d)
    | MoveDefOp(p, sf) -> evalMoveDef p state (getValue sf)
    | NoRet -> state, NoRet
    | SolveOp -> state, evalSolve (state.Add("_player", Player(1)))
    | ValidMoveOp ->
        state,
        (if List.isEmpty (validMoves state) then Bool(false) else Bool(true))

let childGen children =
    if List.isEmpty children then None else Some children

type MaxTree(board: Expr, children: MinTree list, value: int) =
    member this.Board = board
    member this.Children = childGen children
    member this.Player = Player(1)
    member this.Value: int = value

type MinTree(board: Expr, children: MaxTree list, value: int) =
    member this.Board = board
    member this.Children = childGen children
    member this.Player = Player(2)
    member this.Value = value

let getStateVal s state = snd (eval state (Variable(s)))
let getBoard = getStateVal "board"

let getplayer state =
    match getStateVal "_player" state with
    | Player a -> Player a
    | _ -> failwith "_player is not player"

let getp1wins state =
    match getStateVal "p1Wins" state with
    | SavedApp a -> a
    | _ -> failwith "p1wins is not an application"

let getp2wins state =
    match getStateVal "p2Wins" state with
    | SavedApp a -> a
    | _ -> failwith "p2wins is not an application"

let getneitherwins state =
    match getStateVal "_neitherWin" state with
    | SavedApp a -> a
    | _ -> failwith "_neitherWin is not an application"

let getp1moves state =
    match getStateVal "p1Moves" state with
    | SavedApp a -> a
    | _ -> failwith "p1Moves is not an application"

let getp2moves state =
    match getStateVal "p2Moves" state with
    | SavedApp a -> a
    | _ -> failwith "p2Moves is not an application"

let validMoves state =
    if checkWon state then
        []
    else
        match getplayer state with
        | Player(1) -> getList (snd (eval state (getp1moves state)))
        | Player(2) -> getList (snd (eval state (getp2moves state)))
        | _ -> failwith "not a valid player"

let getBoardValue state =
    if (snd (eval state (getp1wins state))) = Bool(true)
    then 1
    elif (snd (eval state (getp2wins state))) = Bool(true)
    then -1
    elif (snd (eval state (getneitherwins state))) = Bool(true)
    then 0
    else failwith "Win Conditions Must Be Mutually Exclusive and Total"

let checkWon state =
    (snd (eval state (getp1wins state))) = Bool(true) || (snd (eval state (getp2wins state))) = Bool(true)

let minValue (children: MaxTree list option) state: int =
    match children with
    | Some l ->
        List.fold (fun acc (x: MaxTree) ->
            if x.Value < acc then x.Value else acc) 1 l
    | None -> getBoardValue state

let maxValue (children: MinTree list option) state: int =
    match children with
    | Some l ->
        List.fold (fun acc (x: MinTree) ->
            if x.Value > acc then x.Value else acc) -1 l
    | None -> getBoardValue state

let genMaxTree (state: Map<string, Expr>) =
    let board = getBoard state

    let rec createChildrenMax l acc maxVal =
        match l with
        | [] -> acc
        | h :: t ->
            let child: MinTree = (genMinTree ((state.Add("board", h)).Add("_player", (Player(2)))))
            if child.Value = 1
            then [ child ]
            else if child.Value <= maxVal
            then (createChildrenMax t acc maxVal)
            else child :: (createChildrenMax t acc child.Value)

    let children = createChildrenMax (validMoves state) [] -2
    MaxTree(board, children, maxValue (childGen children) state)

let genMinTree state =
    let board = getBoard state

    let rec createChildrenMin l acc minVal =
        match l with
        | [] -> acc
        | h :: t ->
            let child: MaxTree = (genMaxTree (state.Add("board", h).Add("_player", (Player(1)))))
            if child.Value = -1
            then [ child ]
            else if child.Value >= minVal
            then (createChildrenMin t acc minVal)
            else child :: (createChildrenMin t acc child.Value)

    let children = createChildrenMin (validMoves state) [] 2
    MinTree(board, children, minValue (childGen children) state)

let evalSolve state: Expr =
    let gameTree = genMaxTree state
    match gameTree.Children with
    | Some c ->
        let solution =
            (List.fold (fun (acc: MinTree) (x: MinTree) ->
                if x.Value > acc.Value then x else acc) c.[0] c)
        match gameTree.Value with
        | 1 ->
            printfn "You can force a win. Solution: %s\n" (prettyPrint solution.Board)
        | 0 ->
            printfn "You can force a draw. Solution:\n%s\n" (prettyPrint solution.Board)
        | -1 -> printfn "Your opponent can force a win. No suggested move."
        | _ -> failwith "Invalid Value"
    | None -> printfn "No Possible Moves"
    NoRet
