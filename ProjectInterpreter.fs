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
    let rec eqHelper (fv: int) (ov: bool) (nums: Expr list) (isInt: bool) =
        if List.isEmpty nums then
            true
        else
            match List.head nums with
            | Num n -> isInt && (n = fv) && eqHelper fv false (List.tail nums) true
            | Bool n -> (not isInt) && (n = ov) && eqHelper 0 ov (List.tail nums) false
            | _ -> failwith ("Eq error: not a bool/int: " + prettyPrint (List.head nums))
    if List.isEmpty nums then
        failwith "Eq takes at least one argument"
    else
        match List.head nums with
        | Num n -> (eqHelper n false (List.tail nums) true)
        | Bool b -> (eqHelper 0 b (List.tail nums) false)
        | _ -> failwith ("Invalid number: " + prettyPrint (List.head nums))

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
    let intNums = getNums nums
    match op with
    | "*" -> Num(evalMultiply intNums)
    | "+" -> Num(evalAdd intNums)
    | "/" -> Num(evalDivide intNums)
    | "-" -> Num(evalSubtract intNums)
    | "<" -> Bool(evalLt intNums)
    | ">" -> Bool(evalGt intNums)
    | "<=" -> Bool(evalLte intNums)
    | ">=" -> Bool(evalGte intNums)
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

let rec eval e =
    match e with
    | Num n -> Num n
    | Bool b -> Bool b
    | Player n -> Player n
    | SavedApp f -> SavedApp f
    | Application(ex, el) -> evalApp (eval ex :: List.map eval el)
    | Variable s -> Variable s
    | AndOp al -> Bool(evalAnd (List.map eval al))
    | OrOp ol -> Bool(evalOr (List.map eval ol))
    | IfOp(i, t, e) -> eval (evalIf (eval i, t, e))
    | NotOp o -> Bool(evalNot (eval o))
    | Program p -> Program(List.map eval p)
    | List l -> List(List.map eval l)
    | Operation o -> Operation o
    | ValOp(i, b) -> evalVal (getNum (eval i)) (getList (eval b))
