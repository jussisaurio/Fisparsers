module CTokenizer
// https://norasandler.com/2017/11/29/Write-a-Compiler.html

open Tokenizer

let CKeywords = Set.ofList ["int"; "return"]

type CToken =
    OpenCurly
    | CloseCurly
    | OpenParen
    | CloseParen
    | Semicolon
    | BitwiseComplement
    | LogicalNegation
    | Plus
    | Minus
    | Multi
    | Div
    | And
    | Or
    | Eq
    | Neq
    | Gt
    | Lt
    | Gte
    | Lte
    | Assignment
    | Keyword of string
    | Identifier of string
    | Integer of int

let (|MatchIdentifier|_|) s =
    matchRegex "^[a-zA-Z]\w*" s

let (|MatchInteger|_|) s =
    Option.map int (matchRegex "^[0-9]+" s)

let (|MatchKeyword|_|) s =
    Option.bind (fun word -> if Set.contains word CKeywords then Some word else None ) (matchRegex "^[a-zA-Z]\w*" s)


let tokenize str =
    let rec _tokenize acc (str: string) l c =
        let prepend t = (t, { Line = l; Column = c })::acc in
        match str with
            | "" -> Ok acc
            | Newline rest -> _tokenize acc rest (l+1) 0
            | EatWhitespace rest -> _tokenize acc rest l (c+1)
            | Prefix "&&" rest -> _tokenize (prepend And) rest l (c+2)
            | Prefix "||" rest -> _tokenize (prepend Or) rest l (c+2)
            | Prefix "==" rest -> _tokenize (prepend Eq) rest l (c+2)
            | Prefix "!=" rest -> _tokenize (prepend Neq) rest l (c+2)
            | Prefix ">=" rest -> _tokenize (prepend Gte) rest l (c+2)
            | Prefix "<=" rest -> _tokenize (prepend Lte) rest l (c+2)
            | Prefix "=" rest -> _tokenize (prepend Assignment) rest l (c+1)
            | Prefix ">" rest -> _tokenize (prepend Gt) rest l (c+1)
            | Prefix "<" rest -> _tokenize (prepend Lt) rest l (c+1)
            | Prefix "{" rest -> _tokenize (prepend OpenCurly) rest l (c+1)
            | Prefix "}" rest -> _tokenize (prepend CloseCurly) rest l (c+1)
            | Prefix "(" rest -> _tokenize (prepend OpenParen) rest l (c+1)
            | Prefix ")" rest -> _tokenize (prepend CloseParen) rest l (c+1)
            | Prefix ";" rest -> _tokenize (prepend Semicolon) rest l (c+1)
            | Prefix "~" rest -> _tokenize (prepend BitwiseComplement) rest l (c+1)
            | Prefix "!" rest -> _tokenize (prepend LogicalNegation) rest l (c+1)
            | Prefix "+" rest -> _tokenize (prepend Plus) rest l (c+1)
            | Prefix "-" rest -> _tokenize (prepend Minus) rest l (c+1)
            | Prefix "*" rest -> _tokenize (prepend Multi) rest l (c+1)
            | Prefix "/" rest -> _tokenize (prepend Div) rest l (c+1)
            | MatchKeyword kw -> let offset = String.length kw in _tokenize (Keyword kw |> prepend) (str.Substring offset) l (c+offset)
            | MatchInteger n -> let offset = if n < 10 then 1 else n |> double |> log10 |> int |> (+) 1 in _tokenize (Integer n |> prepend) (str.Substring offset) l (c+offset)
            | MatchIdentifier ident -> let offset = String.length ident in _tokenize (Identifier ident |> prepend) (str.Substring offset) l (c+offset)
            | invalid -> "Failed to parse: " + invalid |> TokenizeError |> Error

    match _tokenize List.empty str 1 1 with
        | Ok tokens -> tokens |> List.rev |> Ok
        | Error (TokenizeError e) -> Error e