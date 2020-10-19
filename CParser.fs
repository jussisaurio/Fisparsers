module CParser

open Parser
open Tokenizer
open CTokenizer

type Precedence1 = OrOp
type Precedence2 = AndOp
type Precedence3 = EqOp | NeqOp
type Precedence4 = GtOp | GteOp | LtOp | LteOp
type Precedence5 = AddOp | SubOp
type Precedence6 = DivOp | MulOp
type Precedence7 = NegationOp | BitwiseComplOp | LogicalNegationOp

// These handle operator precedence correctly. OR < AND < EQ/NEQ < GT/GTE/LT/LTE < ADD/SUB < MUL/DIV < PARENTHESIZED/UNARY/CONSTANT
type Expr = Expr of Expr2 * (Precedence1 * Expr2) list
and Expr2 = Expr2 of Expr3 * (Precedence2 * Expr3) list
and Expr3 = Expr3 of Expr4 * (Precedence3 * Expr4) list
and Expr4 = Expr4 of Expr5 * (Precedence4 * Expr5) list
and Expr5 = Expr5 of Expr6 * (Precedence5 * Expr6) list
and Expr6 = Expr6 of Expr7 * (Precedence6 * Expr7) list
and Expr7 = Parenthesized of Expr | UnaryOp of Precedence7 * Expr7 | Const of int

type Statement = Return of Expr
type Func = Func of string * Statement
type Program = Program of Func

let private parseStructureToken t = Parser (fun tokens ->
    if List.isEmpty tokens then ("Unexpected end of input, expecting " + string t) |> Error
    else
        let (token, meta) = List.head tokens in
            if token = t then Ok((), List.tail tokens)
            else getPosition meta + " - Expected " + string t + ", found " + string token |> Error
)
let private parseOpenCurly = parseStructureToken OpenCurly
let private parseCloseCurly = parseStructureToken CloseCurly
let private parseOpenParen = parseStructureToken OpenParen
let private parseCloseParen = parseStructureToken CloseParen
let private parseSemicolon = parseStructureToken Semicolon

let rec parseExpr =
    parse {
        let! t = parseExpr2
        let opAndExpr6 = parse {
            let! opr = parseExprBinOperator
            let! t2 = parseExpr2
            return (opr, t2)
        }
        let! rest = zeroOrMore opAndExpr6
        return Expr (t, rest)
    }
and parseExpr2 =
    parse {
        let! t = parseExpr3
        let opAndExpr6 = parse {
            let! opr = parseExprBinOperator2
            let! t2 = parseExpr3
            return (opr, t2)
        }
        let! rest = zeroOrMore opAndExpr6
        return Expr2 (t, rest)
    }
and parseExpr3 =
    parse {
        let! t = parseExpr4
        let opAndExpr6 = parse {
            let! opr = parseExprBinOperator3
            let! t2 = parseExpr4
            return (opr, t2)
        }
        let! rest = zeroOrMore opAndExpr6
        return Expr3 (t, rest)
    }
and parseExpr4 =
    parse {
        let! t = parseExpr5
        let opAndExpr6 = parse {
            let! opr = parseExprBinOperator4
            let! t2 = parseExpr5
            return (opr, t2)
        }
        let! rest = zeroOrMore opAndExpr6
        return Expr4 (t, rest)
    }
and parseExpr5 =
    parse {
        let! t = parseExpr6
        let opAndExpr6 = parse {
            let! opr = parseExprBinOperator5
            let! t2 = parseExpr6
            return (opr, t2)
        }
        let! rest = zeroOrMore opAndExpr6
        return Expr5 (t, rest)
    }

and parseExpr6 =
    parse {
        let! f = parseExpr7
        let opAndExpr7 = parse {
            let! opr = parseExprBinOperator6
            let! f2 = parseExpr7
            return (opr, f2)
        }
        let! rest = zeroOrMore opAndExpr7
        return Expr6 (f, rest)
    }

and parseExpr7 =
    parse {
        return! parseParenthesizedExpr
        return! parseConst
        return! parseUnaryOp
    }

and private parseParenthesizedExpr =
    parse {
        do! parseOpenParen
        let! e = parseExpr
        do! parseCloseParen
        return (Parenthesized e)
    }

and private parseUnaryOperator = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (BitwiseComplement, _) -> Ok(BitwiseComplOp, List.tail tokens)
    | Some (LogicalNegation, _) -> Ok(LogicalNegationOp, List.tail tokens)
    | Some (Minus, _) -> Ok(NegationOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseConst = Parser (fun tokens ->
    match List.tryHead tokens with
    | Some (Integer n, _) -> Ok(Const n, List.tail tokens)
    | Some (other, meta) -> getPosition meta + " - Expected constant, found " + string other |> Error
    | None -> Error "Expected constant, got end of input"
    )

and private parseExprBinOperator = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (Or, _) -> Ok(OrOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseExprBinOperator2 = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (And, _) -> Ok(AndOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseExprBinOperator3 = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (Eq, _) -> Ok(EqOp, List.tail tokens)
    | Some (Neq, _) -> Ok(NeqOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseExprBinOperator4 = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (Gt, _) -> Ok(GtOp, List.tail tokens)
    | Some (Gte, _) -> Ok(GteOp, List.tail tokens)
    | Some (Lt, _) -> Ok(LtOp, List.tail tokens)
    | Some (Lte, _) -> Ok(LteOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseExprBinOperator5 = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (Plus, _) -> Ok(AddOp, List.tail tokens)
    | Some (Minus, _) -> Ok(SubOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseExprBinOperator6 = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (Multi, _) -> Ok(MulOp, List.tail tokens)
    | Some (Div, _) -> Ok(DivOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseUnaryOp =
    parse {
        let! o = parseUnaryOperator
        let! f = parseExpr7
        return (UnaryOp (o, f))
    }

let private parseIdentifier = Parser (fun tokens ->
    match List.tryHead tokens with
    | Some (Identifier i, _) -> Ok(i, List.tail tokens)
    | Some (other, meta) -> getPosition meta + " - Expected identifier, found " + string other |> Error
    | None -> Error "Expected identifier, got end of input"
    )

let private parseKeyword k = Parser (fun tokens ->
    match List.tryHead tokens with
    | Some (Keyword kw, meta) -> if kw = k then Ok(kw, List.tail tokens) else getPosition meta + " - Expected keyword, found '" + k + "'" |> Error
    | Some (other, meta) -> getPosition meta + " - Expected keyword, found '" + string other + "'" |> Error
    | None -> Error "Expected keyword, got end of input"
    )

let private parseStatement =
    parse {
        let! _ = parseKeyword "return"
        let! ex = parseExpr
        do! parseSemicolon
        return (Return ex)
    }

let private parseFuncDecl =
    parse {
        let! _ = parseKeyword "int"
        let! fnName = parseIdentifier
        do! parseOpenParen
        do! parseCloseParen
        do! parseOpenCurly
        let! stmt = parseStatement
        do! parseCloseCurly
        return Func(fnName, stmt)
    }

let private parseProgram =
    parse {
        let! main = parseFuncDecl
        return (Program main)
    }

let run (Parser func) = func >> (fun result ->
    match result with
        | Error e -> Error e
        | Ok (j, leftover) -> if List.isEmpty leftover then Ok j else let t = List.head leftover |> string in "Unexpected token " + t |> Error
    )

let parseCProgram input =
    Result.bind (run parseProgram) (tokenize input)