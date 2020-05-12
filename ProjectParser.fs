module ProjectParser

open Parser

type Expr =
    | Num of int
    | Operation of string
    | AndOp of Expr list
    | OrOp of Expr list
    | IfOp of Expr list
    | NotOp of Expr
    | ValOp of Expr * Expr
    | Variable of string
    | Bool of bool
    | Application of Expr * Expr list
    | SavedApp of Expr
    | Player of int
    | Program of Expr list
    | List of Expr list

(* HELPER COMBINATORS *)

(*
 * From Course Material
 * Returns a list of p separated by sep
 * @param p   A parser.
 * @param sep A separator parser.
 *)
let pmany2sep p sep = pseq p (pmany1 (pright sep p)) (fun (x, xs) -> x :: xs) <!> "pmany2sep"

(*
 * From Course Material
 * Accepts whatever p accepts, surrounded
 * by parens, i.e., (p), and returns whatever p
 * returns.
 * @param p A parser.
 *)
let inParens p = pbetween (pchar '(') (pchar ')') p <!> "inParens"

(* Grammar *)

let expr, exprImpl = recparser()

let pTrue = pstr "True" <|> pstr "true" |>> (fun c -> Bool(true)) <!> "pTrue"

let pFalse = pstr "False" <|> pstr "false" |>> (fun c -> Bool(false)) <!> "pFalse"

let pBool = pTrue <|> pFalse <!> "pBool"

// Adapted from Course Material
let pNumber = pseq (pchar '-' <|> pdigit) (pmany0 pdigit) (fun(a,b) -> Num(int (stringify (a::b)))) <!> "pNumber"

let pNeither = pstr "Neither" |>> (fun c -> Player(0)) <!> "Neither"

let pPlayer1 = pstr "Player1" |>> (fun c -> Player(1)) <!> "pPlayer1"

let pPlayer2 = pstr "Player2" |>> (fun c -> Player(2)) <!> "pPlayer2"

let pPlayer = pPlayer1 <|> pPlayer2 <|> pNeither <!> "pPlayer"

let pVariable = pmany1 (pletter <|> pchar '?') |>> (fun c -> Variable(stringify c)) <!> "pVariable"

(*
 * Parses a specific operation in parenthases
 * by parens, i.e., (op a b), and returns a list
 * of parameters.
 * @param op a string representing function name.
 *)
let funCall op = inParens (pright (pstr (op + " ")) (pmany2sep expr pws1)) <!> "funCall"

let pAnd = funCall "and" |>> (fun a -> AndOp(a)) <!> "pAnd"

let pOr = funCall "or" |>> (fun a -> OrOp(a)) <!> "pOr"

let pList =
        inParens (pstr "list") |>> (fun a -> List([]))
        <|> (funCall "list" |>> (fun a -> List(a)))
        <!> "pList"

let pIf =
    funCall "if"
    |>> (fun r ->
                if r.Length = 3
                then IfOp(r)
                else failwith "If statements must have 3 arguments"
        )
    <!> "pIf"

let pVal =
    funCall "val"
    |>> (fun r ->
                if r.Length = 2
                then ValOp(r.[0], r.[1])
                else failwith "val statements must have 2 arguments"
        )
    <!> "pVal"

let pNot = inParens (pright (pstr "not ") expr) |>> (fun a -> NotOp(a)) <!> "pNot"

let pBuiltIn = pAnd <|> pIf <|> pVal <|> pOr <|> pList <|> pNot

let pOp = pstr "+" <|> pstr "/" <|> pstr "-" <|> pstr "*" <|> pstr "=" <|> pstr ">=" <|> pstr "<=" <|> pstr "<" <|> pstr ">" |>> (fun c -> Operation(c)) <!> "pOp"

let pFunctionName = pOp <|> pVariable <!> "pFunctionName"

let pApplication =
    inParens (pseq pFunctionName (pmany0 (pright pws1 expr)) (fun (c, d) -> Application(c, d))) <!> "pApplication"

let pSavedApp = pseq (pchar '\'') (pBuiltIn <|> pApplication) (fun (c, d) -> SavedApp(d)) <!> "pSavedFun"

exprImpl := pBuiltIn <|> pApplication <|> pSavedApp <|> pNumber <|> pPlayer <|> pBool <|> pVariable <!> "expr"

let manyExpr =
    pleft (pseq expr (pmany0 (pright pws1 expr)) (fun (x, xs) -> Program(x :: xs))) pws0 <!> "manyExpr"

let grammar = pright pws0 (pleft manyExpr peof) <!> "grammar"

(*From course material*)
let parse input : Expr option =
    let input' = prepare input
    match grammar input' with
    | Success(res,_)     -> Some res
    | Failure(pos, rule) ->
        printfn "Invalid expression."
        let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos input msg
        printf "%s" diag
        None

let rec prettyPrint ast =
    match ast with
    | Num n -> string n
    | Bool b -> string b
    | Player n -> if n = 1 then "PlayerOne" elif n = 2 then "PlayerTwo" else "Neither"
    | SavedApp f -> "{Saved Application: " + prettyPrint f + " }"
    | Application(ex, el) -> "{Apply " + prettyPrint ex + " to (" + String.concat " " (List.map prettyPrint el) + ")}"
    | Variable s -> s
    | AndOp al -> "{AndOp: " + String.concat " & " (List.map prettyPrint al) + "}"
    | OrOp ol -> "{OrOp: " + String.concat " || " (List.map prettyPrint ol) + "}"
    | IfOp il -> "{IfOp: if: " + prettyPrint il.[0] + ", then: " + prettyPrint il.[1] + ", else: " + prettyPrint il.[2] + "}"
    | NotOp o -> "{Not: " + prettyPrint o + "}"
    | Program p -> String.concat "\n\n" (List.map prettyPrint p)
    | Operation o -> o
    | List l -> "(" + String.concat ", " (List.map prettyPrint l) + ")"
    | _ -> "no print method known"
