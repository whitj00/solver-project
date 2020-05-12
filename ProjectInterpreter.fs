module ProjectInterpreter

open ProjectParser

(* HELPTER FUNCTIONS *)
let getNum n =
    match n with
    | Num n' -> n'
    | _ -> failwith ((prettyPrint n) + " is not a number")

let getList l =
    match l with
    | List l' -> l'
    | _ -> failwith ((prettyPrint l) + " is not a list")

let truthy (v: Expr): bool =
    match v with
    | Bool b -> b
    | Num n -> n > 0
    | _ -> failwith ("Value " + prettyPrint v +  " is not truthy/falsy")

let rec evalAnd (al: Expr list): bool =
    if List.isEmpty al then true
    else if truthy (List.head al) then evalAnd (List.tail al)
    else false

let rec evalOr (ol: Expr list): bool =
    if List.isEmpty ol then false
    else if truthy (List.head ol) then true
    else evalOr (List.tail ol)

let rec evalIf (il: Expr list): Expr =
    if List.length il <> 3
    then failwith "If-statements must have a statement and two clauses"
    else if truthy il.[0]
    then il.[1]
    else il.[2]

let rec evalMultiply nums =
    if List.isEmpty nums then 1
    else nums.[0] * (evalMultiply (List.tail nums))

let rec evalAdd nums =
    if List.isEmpty nums then 0
    else nums.[0] + (evalAdd (List.tail nums))

let rec evalDivide (nums: int list) =
    if List.isEmpty nums then 1
    else List.head nums / evalMultiply (List.tail nums)

let rec evalSubtract nums =
    if List.isEmpty nums then 0
    else nums.[0] - (evalAdd (List.tail nums))

let rec evalEq nums =
    let rec eqHelper (fv : int) (ov : bool) (nums : Expr list) (isInt : bool) =
        if List.isEmpty nums then true
        else
            match List.head nums with
            | Num n -> isInt && (n = fv) && eqHelper fv false (List.tail nums) true
            | Bool n -> (not isInt) && (n = ov) && eqHelper 0 ov (List.tail nums) false
            | _ -> failwith ("Eq error: not a bool/int: " + prettyPrint (List.head nums))
    if List.isEmpty nums then failwith "Eq takes at least one argument"
    else
        match List.head nums with
        | Num n -> (eqHelper n false (List.tail nums) true)
        | Bool b -> (eqHelper 0 b (List.tail nums) false)
        | _ -> failwith ("Invalid number: " + prettyPrint (List.head nums))

let rec evalCompare (nums: int list) op: bool =
    if List.length nums < 2 then true
    else (op nums.[0] nums.[1]) && (evalCompare (List.tail nums) op)

let evalLt nums = evalCompare nums (<)
let evalGt nums = evalCompare nums (>)
let evalLte nums = evalCompare nums (<=)
let evalGte nums = evalCompare nums (>=)

let evalMath op nums =
    let intNums = List.map getNum nums
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

let evalApp a: Expr =
    match List.head a with
    | Variable v ->
        printf "Pretending to run %s\n" v
        Application(a.Head, a.Tail)
    | Operation o -> evalMath o (List.tail a)
    | _ -> failwith "Invalid Function Name"

let evalNot op : bool =
    not (truthy op)

let evalVal index board =
    if index < List.length board then board.[index]
    else failwith ("Index " + string index + " is out-of-bounds")

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
    | IfOp il -> eval (evalIf ((eval (List.head il)) :: (List.tail il)))
    | NotOp o -> Bool(evalNot (eval o))
    | Program p -> Program(List.map eval p)
    | List l -> List(List.map eval l)
    | Operation o -> Operation o
    | ValOp (i, b) -> evalVal (getNum (eval i)) (getList (eval b))
