module JSONTokenizer

open System.Text.RegularExpressions
open Tokenizer

type JSONToken = ObjectStart | ObjectEnd | ArrayStart | ArrayEnd | KeyEnd | Comma | JString of string | JNumber of float | JNull | JTrue | JFalse

let stringToken (str: string) =
    if str.StartsWith("\"") |> not then None
    else
        let rec findIndex (i: int) (substr: string) =
            let endQ = substr.IndexOf("\"")
            match endQ with
                | -1 -> None
                | j -> if substr.[j-1] = '\\' then findIndex (i+j+1) (substr.Substring(j+1)) else Some (i+j)

        let rest = str.Substring(1)
        match findIndex 1 rest with
            | None -> None
            | Some endIndex -> Some (JString str.[1..endIndex-1], str.Substring(endIndex+1))

let private numberToken str =
    let m = Regex.Match(str, "^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")
    if not m.Success then None
    else
        let mtc = m.Groups.[0].Value in
        match System.Double.TryParse(mtc) with
            | true, n -> Some (JNumber n, str.Substring(mtc.Length))
            | false, _ -> None

let private nullOrBoolToken str =
    if String.length str >= 4 && str.[0..3] = "null" then Some (JNull, str.[4..])
    else if String.length str >= 4 && str.[0..3] = "true" then Some (JTrue, str.[4..])
    else if String.length str >= 5 && str.[0..4] = "false" then Some (JFalse, str.[5..])
    else None

type private MatchFirstBuilder() =
    member this.ReturnFrom(x) = x
    member this.Combine (a,b) =
        match a with
        | Some _ -> a
        | None -> b
    member this.Delay(f) = f()
    member this.Zero() =
        None

let private first = MatchFirstBuilder()

let tokenize str =
    let rec _tokenize acc (str: string) l c =
        let prepend t = (t, { Line = l; Column = c })::acc in
        match str with
            | "" -> Ok acc
            | Newline rest -> _tokenize acc rest (l+1) 0
            | EatWhitespace rest -> _tokenize acc rest l (c+1)
            | Prefix "{" rest -> _tokenize (prepend ObjectStart) rest l (c+1)
            | Prefix "}" rest -> _tokenize (prepend ObjectEnd) rest l (c+1)
            | Prefix "[" rest -> _tokenize (prepend ArrayStart) rest l (c+1)
            | Prefix "]" rest -> _tokenize (prepend ArrayEnd) rest l (c+1)
            | Prefix ":" rest -> _tokenize (prepend KeyEnd) rest l (c+1)
            | Prefix "," rest -> _tokenize (prepend Comma) rest l (c+1)
            | x -> match first
                    {
                        return! stringToken x
                        return! numberToken x
                        return! nullOrBoolToken x
                    } with
                      | Some (token, rest) -> let offset = String.length str - String.length rest in _tokenize(prepend token) rest l (c + offset)
                      | _ -> TokenizeError "wat" |> Error

    match _tokenize List.empty str 1 1 with
        | Ok tokens -> tokens |> List.rev |> Ok
        | e -> e