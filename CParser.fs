module CParser

open Parser
open Tokenizer
open CTokenizer

type Expr = Const of int | Unary of UnaryOperator * Expr
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

let rec private parseExpr = Parser (fun tokens ->
    let rec parse ts =
        match List.tryHead ts with
        | Some (Integer n, _) -> Ok(Const n, List.tail ts)
        | Some (UnaryOp u, _) -> List.tail ts |> parse >>= (fun (subExpr, tail) -> Ok(Unary (u,subExpr), tail))
        | Some (other, meta) -> getPosition meta + " - Expected expression, found " + string other |> Error
        | None -> Error "Expected expression, got end of input"
    parse tokens
    )

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