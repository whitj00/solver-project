module ProjectParser

open Parser

type Expr =
    | Num of int
    | Operation of char
    | AndOp of Expr list
    | OrOp of Expr list
    | IfOp of Expr list
    | Variable of string
    | Bool of bool
    | Application of Expr * Expr list
    | SavedFun of Expr
    | Player of int
    | Program of Expr list

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

(* From Course Material *)
let pNumber = pmany1 pdigit |>> (fun ds -> Num(int (stringify ds))) <!> "pNumber"

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

let pIf = funCall "if" |>> (fun r -> IfOp(r)) <!> "pIf"

let pSpecialForm = pAnd <|> pIf <|> pOr

let pOp = pchar '+' <|> pchar '/' <|> pchar '-' <|> pchar '*' <|> pchar '=' |>> (fun c -> Operation(c)) <!> "pOp"


let pFunctionName = pOp <|> pVariable <!> "pFunctionName"

let pApplication =
    inParens (pseq pFunctionName (pmany0 (pright pws1 expr)) (fun (c, d) -> Application(c, d))) <!> "pApplication"

let pSavedFun = pseq (pchar '\'') pApplication (fun (c, d) -> SavedFun(d)) <!> "pSavedFun"

exprImpl := pSpecialForm <|> pApplication <|> pSavedFun <|> pNumber <|> pPlayer <|> pBool <|> pVariable <!> "expr"

let manyExpr =
    pleft (pseq expr (pmany0 (pright pws1 expr)) (fun (x, xs) -> Program(x :: xs))) pws0 <!> "manyExpr"


let grammar = pleft manyExpr peof <!> "grammar"
