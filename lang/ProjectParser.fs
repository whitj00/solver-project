module ProjectParser

open Parser

type Expr =
    | Num of int
    | Operation of string
    | AndOp of Expr list
    | OrOp of Expr list
    | IfOp of Expr * Expr * Expr
    | NotOp of Expr
    | ValOp of Expr * Expr
    | LenOp of Expr
    | Variable of string
    | Bool of bool
    | Application of Expr * Expr list
    | SavedApp of Expr
    | Player of int
    | Program of Expr list
    | List of Expr list
    | WinDefOp of Expr * Expr
    | ChangeOp of Expr * Expr * Expr
    | AppendOp of Expr list
    | BoardDefOp of Expr
    | MoveDefOp of Expr * Expr
    | NoRet
    | SolveOp
    | ValidMoveOp

(* HELPER FUNCTIONS *)
let makeNum (n: int) = Num n
let makeVar (v: string) = Variable v

(* HELPER COMBINATORS *)

(*
 * From Course Material
 * Returns a list of p separated by sep
 * @param p   A parser.
 * @param sep A separator parser.
 *)
let pmany2sep p sep = pseq p (pmany0 (pright sep p)) (fun (x, xs) -> x :: xs) <!> "pmany2sep"

(*
 * From Course Material
 * Accepts whatever p accepts, surrounded
 * by parens, i.e., (p), and returns whatever p
 * returns.
 * @param p A parser.
 *)
let inParens p = pbetween (pchar '(') (pright pws0 (pchar ')')) p <!> "inParens"


(* Comment Parsing*)
let is_not_pound (c: char) = is_regexp (c.ToString()) @"[^#]"
let pComment =
    pbetween ((pstr "###") <|> (pstr "#")) ((pstr "###") <|> (pstr "#")) (pmany0 (psat is_not_pound))
    |>> (fun c -> NoRet) <!> "inParens"

(* Grammar *)

let expr, exprImpl = recparser()

let pTrue = pstr "True" <|> pstr "true" |>> (fun c -> Bool(true)) <!> "pTrue"

let pFalse = pstr "False" <|> pstr "false" |>> (fun c -> Bool(false)) <!> "pFalse"

let pBool = pTrue <|> pFalse <!> "pBool"

// Adapted from Course Material
let pPositiveNumber = pmany1 pdigit <!> "pPositiveNumber"

let pNegativeNumber = pright (pchar '-') pPositiveNumber |>> (fun ds -> '-' :: ds) <!> "pNegativeNumber"

let pNumber =
    (pPositiveNumber <|> pNegativeNumber) |>> (stringify
                                               >> int
                                               >> makeNum)
    <!> "pNumber"

let pNeither = pstr "Neither" |>> (fun c -> Player(0)) <!> "Neither"

let pPlayer1 = pstr "Player1" |>> (fun c -> Player(1)) <!> "pPlayer1"

let pPlayer2 = pstr "Player2" |>> (fun c -> Player(2)) <!> "pPlayer2"

let pPlayer = pPlayer1 <|> pPlayer2 <|> pNeither <!> "pPlayer"

let pVariable = pmany1 (pletter <|> pchar '?') |>> (stringify >> makeVar) <!> "pVariable"

(*
 * Parses a specific operation in parenthases
 * by parens, i.e., (op a b), and returns a list
 * of parameters.
 * @param op a string representing function name.
 *)
let funCall op = inParens (pright (pleft (pstr (op + " ")) pws0) (pmany2sep expr pws1)) <!> "funCall"

let pAnd = funCall "and" |>> AndOp <!> "pAnd"

let pOr = funCall "or" |>> OrOp <!> "pOr"

let pList = inParens (pstr "list") |>> (fun a -> List([])) <|> (funCall "list" |>> List) <!> "pList"

let pSolve = inParens (pstr "solve") |>> fun a -> SolveOp

let pValidMoves = inParens (pstr "validMoves?") |>> fun a -> ValidMoveOp

let pLen =
    funCall "len" |>> (fun r ->
    match List.length r with
    | 1 -> LenOp(List.head r)
    | _ -> failwith "Length statements must have 1 argument")
    <!> "pLen"

let pIf =
    funCall "if" |>> (fun r ->
    match r.Length with
    | 3 -> IfOp(r.[0], r.[1], r.[2])
    | _ -> failwith "If statements must have 3 arguments")
    <!> "pIf"

let pWinDef =
    funCall "defWin" |>> (fun r ->
    match r.Length with
    | 2 -> WinDefOp(r.[0], r.[1])
    | _ -> failwith "defWin statements must have 2 arguments")
    <!> "pWinDef"

let pBoardDef =
    funCall "defBoard" |>> (fun r ->
    match r.Length with
    | 1 -> BoardDefOp(r.Head)
    | _ -> failwith "defBoard statements must have 1 argument")
    <!> "pBoardDef"


let pMoveDef =
    funCall "defMoves" |>> (fun r ->
    match r.Length with
    | 2 -> MoveDefOp(r.[0], r.[1])
    | _ -> failwith "defMove statements must have 2 arguments")
    <!> "pMoveDef"

let pChanges =
    funCall "changeList" |>> (fun r ->
    match r.Length with
    | 3 -> ChangeOp(r.[0], r.[1], r.[2])
    | _ -> failwith "changeList statements must have 2 arguments")
    <!> "pChangeList"

let pAppend = funCall "append" |>> AppendOp <!> "pAppend"

let pVal =
    funCall "val" |>> (fun r ->
    match r.Length with
    | 2 -> ValOp(r.[0], r.[1])
    | _ -> failwith "val statements must have 2 arguments")
    <!> "pVal"

let pNot = inParens (pright (pstr "not ") expr) |>> (fun a -> NotOp(a)) <!> "pNot"

let pBuiltIn =
    pAnd <|> pIf <|> pVal <|> pOr <|> pList <|> pNot <|> pLen <|> pWinDef <|> pChanges <|> pAppend <|> pBoardDef
    <|> pValidMoves <|> pSolve <|> pMoveDef <!> "pBuiltIn"

let pOp =
    pstr "+" <|> pstr "/" <|> pstr "-" <|> pstr "*" <|> pstr "=" <|> pstr ">=" <|> pstr "<=" <|> pstr "<" <|> pstr ">"
    |>> (fun c -> Operation(c)) <!> "pOp"

let pFunctionName = pOp <|> pVariable <!> "pFunctionName"

let pApplication =
    inParens (pseq pFunctionName (pmany0 (pright pws1 expr)) (fun (c, d) -> Application(c, d))) <!> "pApplication"

let pSavedApp = pseq (pchar '\'') (pBuiltIn <|> pApplication) (fun (c, d) -> SavedApp(d)) <!> "pSavedFun"

exprImpl := pBuiltIn <|> pApplication <|> pSavedApp <|> pNumber <|> pPlayer <|> pBool <|> pVariable <!> "expr"

let manyExpr =
    pleft (pseq (pComment <|> expr) (pmany0 (pright pws1 (pComment <|> expr))) (fun (x, xs) -> Program(x :: xs))) pws0
    <!> "manyExpr"

let grammar = pright pws0 (pleft manyExpr peof) <!> "grammar"

(*From course material*)
let parse input: Expr option =
    let input' = prepare input
    match grammar input' with
    | Success(res, _) -> Some res
    | Failure(pos, rule) ->
        printfn "Invalid expression."
        let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos input msg
        printf "%s" diag
        None

let rec prettyPrint ast =
    let prettyHelper argList =
        "(" + (String.concat " " argList) + ")"
    match ast with
    | Num n -> string n
    | Bool b -> string b
    | Operation o -> o
    | Variable s -> s
    | Player 1 -> "Player1"
    | Player 2 -> "Player2"
    | Player _ -> "Neither"
    | List l ->
        let argList = List.map prettyPrint l
        "[" + (String.concat "; " argList) + "]"
    | SavedApp f -> "'" + prettyPrint f
    | Application(ex, el) ->
        let argList = List.map prettyPrint (ex::el)
        prettyHelper argList
    | AndOp al -> 
        let argList = "and"::(List.map prettyPrint al)
        prettyHelper argList
    | OrOp ol ->
        let argList = "or"::(List.map prettyPrint ol)
        prettyHelper argList
    | NotOp o ->
        let argList = "not"::[prettyPrint o]
        prettyHelper argList
    | Program p ->
        String.concat "\n\n" (List.map prettyPrint p)
    | AppendOp l ->
        let argList = "append"::(List.map prettyPrint l)
        prettyHelper argList
    | ChangeOp _ -> "Change"
    | IfOp (i, t, e) ->
        let argList = "if"::[prettyPrint i; prettyPrint t; prettyPrint e]
        prettyHelper argList
    | LenOp b ->
        let argList = "len"::[prettyPrint b]
        prettyHelper argList
    | ValOp (i, b) ->
        let argList = "val"::[prettyPrint i; prettyPrint b]
        prettyHelper argList
    | WinDefOp _ -> "defWin"
    | BoardDefOp _ -> "defBoard"
    | MoveDefOp _ -> "defMoves"
    | SolveOp _ -> "(solve)"
    | NoRet -> ""
    | ValidMoveOp -> "(validMoves?)"

