module CParser

open Parser
open Tokenizer
open CTokenizer

type PlusMinus = PlusOp | MinusOp
type DivMulti = DivOp | MultiOp

type UnOp = NegationOp | BitwiseComplOp | LogicalNegationOp

// These handle operator precedence correctly. Associativity of +/-, and precedence: Unary > Div/Multi > Plus/Minus
type Expr = Expr of Term * (PlusMinus * Term) list
and Term = Term of Factor * (DivMulti * Factor) list
and Factor = Parenthesized of Expr | UnaryOp of UnOp * Factor | Const of int

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

// Parsing an expression means:
// 1. Parsing a term 2. Maybe parsing (+,-) and another term, 3. repeat 2 until fail
let rec parseExpr =
    parse {
        let! t = parseTerm
        let opAndTerm = parse {
            let! opr = parsePlusMinus
            let! t2 = parseTerm
            return (opr, t2)
        }
        let! rest = zeroOrMore opAndTerm
        return Expr (t, rest)
    }

// Parsing a term means:
// 1. Parsing a factor 2. Maybe parsing (*,/) and another factor, 3. repeat 2 until
and private parseTerm =
    parse {
        let! f = parseFactor
        let opAndFactor = parse {
            let! opr = parseMulDiv
            let! f2 = parseFactor
            return (opr, f2)
        }
        let! rest = zeroOrMore opAndFactor
        return Term (f, rest)
    }

// Parsing a factor means:
// 1. Parsing a parenthesized expression, OR 2. parsing a constant, OR 3. parsing a unary operation
and private parseFactor =
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

and private parsePlusMinus = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (Plus, _) -> Ok(PlusOp, List.tail tokens)
    | Some (Minus, _) -> Ok(MinusOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseMulDiv = Parser (fun tokens ->
    match List.tryHead tokens with
    | None -> Error "uhh"
    | Some (Multi, _) -> Ok(MultiOp, List.tail tokens)
    | Some (Div, _) -> Ok(DivOp, List.tail tokens)
    | _ -> Error "uhh"
    )

and private parseUnaryOp =
    parse {
        let! o = parseUnaryOperator
        let! f = parseFactor
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