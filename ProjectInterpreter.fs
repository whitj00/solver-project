module ProjectInterpreter

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


//maybe add state as a parameter here
let rec eval e =
    match e with
    | Num n -> Num n
    | Bool b -> Bool b
    | Player n -> Player n
    | SavedApp f -> SavedApp f
    | Variable s -> evalVar s
    | Application(ex, el) -> evalApp (eval ex :: List.map eval el)
    | AndOp al -> Bool(evalAnd (List.map eval al))
    | OrOp ol -> Bool(evalOr (List.map eval ol))
    | IfOp(i, t, e) -> eval (evalIf (eval i, t, e))
    | NotOp o -> Bool(evalNot (eval o))
    | LenOp l -> Num(evalLen (eval l))
    | Program p -> Program(List.map eval p)
    | List l -> List(List.map eval l)
    | Operation o -> Operation o
    | ValOp(i, b) -> evalVal (getNum (eval i)) (getList (eval b))
    | WinDef(p, sf) -> WinDef(p, sf)
    | ChangeOp(i, c, b) -> List(evalAllChanges (eval i) (eval c) (eval b))
    | AppendOp l -> List(evalAppend (List.map (eval >> getList) l))
