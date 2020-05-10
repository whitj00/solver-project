module ProjectInterpreter

open ProjectParser

let truthy (v : Expr) : bool =
    match v with
    | Bool b -> b
    | Num n -> n > 0
    | _ -> failwith "Value is not truthy/falsy"
 
let rec evalAnd (al : Expr list) : bool =
    if List.length al = 0 then
        true
    else
        if truthy (List.head al) then
            evalAnd (List.tail al)
        else false

let rec evalOr (ol : Expr list) : bool =
    if List.length ol = 0 then
        false
    else
        if truthy (List.head ol) then true
        else evalOr (List.tail ol)

let rec evalIf (il : Expr list) : Expr =
    if List.length il <> 3 then
        failwith "If-statements must have a statement and two clauses"
    else
        if truthy il.[0] then il.[1]
        else il.[2]

let rec multiply nums = 
    if List.isEmpty nums then 1
    else match List.head nums with
    | Num n -> n * (multiply (List.tail nums))
    | _ -> failwith "Invalid number"

let rec add nums = 
    if List.isEmpty nums then 0
    else match List.head nums with
    | Num n -> n + (multiply (List.tail nums))
    | _ -> failwith "Invalid number"

let rec divide nums = 
    if List.isEmpty nums then 1
    else match List.head nums with
    | Num n -> n / (divide (List.tail nums))
    | _ -> failwith "Invalid number"

let rec subtract nums = 
    if List.isEmpty nums then 0
    else match List.head nums with
    | Num n -> n - (subtract (List.tail nums))
    | _ -> failwith "Invalid number"

let rec eq nums =
    let rec eqHelper fv nums =
        if List.isEmpty nums then true
        else match List.head nums with
        | Num n -> (n = fv) && eqHelper fv (List.tail nums)
        | _ -> failwith "Invalid number"
    if List.isEmpty nums then failwith "Eq takes at least one argument"
    else match List.head nums with
    | Num n -> (eqHelper n (List.tail nums))
    | _ -> failwith "Invalid number"


let evalMath op nums =
    if op = '*' then Num(multiply nums)
    elif op = '+' then Num(add nums)
    elif op = '/' then Num(divide nums)
    elif op = '-' then Num(subtract nums)
    elif op = '=' then Bool(eq nums)
    else failwith "Invalid Operation"
    
let evalApp a : Expr =
    match List.head a with
    | Variable v ->
        printf "Pretending to run %s" v
        Application (a.Head, a.Tail)
    | Operation o -> evalMath o (List.tail a)
    | _ -> failwith "Invalid Function Name"


let rec eval e =
    match e with
    | Num n -> Num n
    | Bool b -> Bool b
    | Player n -> Player n
    | SavedFun f -> SavedFun f
    | Application(ex, el) -> evalApp(eval ex::List.map eval el)
    | Variable s -> Variable s
    | AndOp al -> Bool (evalAnd (List.map eval al))
    | OrOp ol -> Bool (evalOr (List.map eval ol))
    | IfOp il -> eval (evalIf ((eval (List.head il))::(List.tail il)))
    | Program p -> Program(List.map eval p)
    | Operation o -> Operation o