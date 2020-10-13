module Tokenizer
open System.Text.RegularExpressions

type TokenMeta = {
    Line: int
    Column: int
}
type Token<'T> = 'T * TokenMeta
type TokenizeError = TokenizeError of string

let getPosition tm = "Line " + string tm.Line + ", Column " + string tm.Column

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let matchRegex (p:string) (s:string) =
    let m = Regex.Match(s, p)
    if not m.Success then None else Some m.Groups.[0].Value

let (|Newline|_|) (s: string) =
    if String.length s >= 2 && s.[0..1] = "\r\n" then Some(s.Substring(2))
    else if s.[0] = '\n' then Some(s.Substring(1))
    else None

let (|EatWhitespace|_|) (s:string) = if System.String.IsNullOrWhiteSpace s.[0..0] then s.[1..] |> Some else None