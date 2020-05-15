module ProjectInterpreter

open ProjectParser

let state = Map<string, Expr>

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
    | Variable v ->
        printf "Pretending to run %s\n" v
        Application(a.Head, a.Tail)
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
let evalVar name =
    match name with
    | "board" ->
        List
            ([ Num 1
               Num 1
               Num 1
               Num 0
               Num 0
               Num 0
               Num 0
               Num 0
               Num 0 ])
    | _ -> Variable name

let rec evalProgram state expr exprAcc : Expr list =
    if List.isEmpty expr then exprAcc
    else
        let (newState, ret) = eval state (List.head expr)
        evalProgram newState (List.tail expr) (exprAcc@[ret])

and eval (state: Expr list) (otherParam: Expr) =
    let getValue expr = snd (eval state expr)
    match otherParam with
    | Num n -> state, Num n
    | Bool b -> state, Bool b
    | Player n -> state, Player n
    | SavedApp f -> state, SavedApp f
    | Variable s -> state, evalVar s
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
    | WinDef(p, sf) -> state, WinDef(getValue p, getValue sf)
    | ChangeOp(i, c, b) -> state, List(evalAllChanges (getValue i) (getValue c) (getValue b))
    | AppendOp l -> state, List(evalAppend (List.map (getValue >> getList) l))