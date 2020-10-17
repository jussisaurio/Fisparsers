module CTokenizer
// https://norasandler.com/2017/11/29/Write-a-Compiler.html

open Tokenizer

let CKeywords = Set.ofList ["int"; "return"]

type UnaryOperator = Negation | BitwiseComplement | LogicalNegation
type CToken =
    OpenCurly
    | CloseCurly
    | OpenParen
    | CloseParen
    | Semicolon
    | UnaryOp of UnaryOperator
    | Keyword of string
    | Identifier of string
    | Integer of int

let (|MatchIdentifier|_|) s =
    matchRegex "^[a-zA-Z]\w+" s

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
            | Prefix "{" rest -> _tokenize (prepend OpenCurly) rest l (c+1)
            | Prefix "}" rest -> _tokenize (prepend CloseCurly) rest l (c+1)
            | Prefix "(" rest -> _tokenize (prepend OpenParen) rest l (c+1)
            | Prefix ")" rest -> _tokenize (prepend CloseParen) rest l (c+1)
            | Prefix ";" rest -> _tokenize (prepend Semicolon) rest l (c+1)
            | Prefix "-" rest -> _tokenize (prepend (UnaryOp Negation)) rest l (c+1)
            | Prefix "~" rest -> _tokenize (prepend (UnaryOp BitwiseComplement)) rest l (c+1)
            | Prefix "!" rest -> _tokenize (prepend (UnaryOp LogicalNegation)) rest l (c+1)
            | MatchKeyword kw -> let offset = String.length kw in _tokenize (Keyword kw |> prepend) (str.Substring offset) l (c+offset)
            | MatchInteger n -> let offset = n |> double |> log10 |> int |> (+) 1 in _tokenize (Integer n |> prepend) (str.Substring offset) l (c+offset)
            | MatchIdentifier ident -> let offset = String.length ident in _tokenize (Identifier ident |> prepend) (str.Substring offset) l (c+offset)
            | invalid -> TokenizeError invalid |> Error

    match _tokenize List.empty str 0 0 with
        | Ok tokens -> tokens |> List.rev |> Ok
        | Error (TokenizeError e) -> Error e