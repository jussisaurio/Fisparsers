module JSONParser

open Parser
open Tokenizer
open JSONTokenizer

type JSONKey = JSONKey of string
type JSON = String of string | Number of float | Null | Boolean of bool | Array of JSON list | Object of Map<JSONKey,JSON>

let private parseStructureToken t = Parser (fun tokens ->
    if List.isEmpty tokens then ("Unexpected end of input, expecting " + string t) |> Error
    else
        let (token, meta) = List.head tokens in
            if token = t then Ok((), List.tail tokens)
            else getPosition meta + " - Expected " + string t + ", found " + string token |> Error
)
let private parseArrayStart = parseStructureToken ArrayStart
let private parseArrayEnd = parseStructureToken ArrayEnd
let private parseObjectStart = parseStructureToken ObjectStart
let private parseObjectEnd = parseStructureToken ObjectEnd
let private parseComma = parseStructureToken Comma

let private parseLiteral = Parser (fun tokens ->
    match tokens with
        | [] -> Error "Unexpected end of input"
        | (JNull, _)::xs -> Ok (Null, xs)
        | (JTrue, _)::xs -> Ok (Boolean true, xs)
        | (JFalse, _)::xs -> Ok (Boolean false, xs)
        | (JString s, _)::(KeyEnd, _)::xs -> "Unexpected key " + s |> Error
        | (JString s, _)::xs -> Ok (String s, xs)
        | (JNumber n, _)::xs -> Ok (Number n, xs)
        | (invalid, meta)::_ -> getPosition meta + " - Unexpected token " + string invalid |> Error
    )

let private parseKey = Parser (fun tokens ->
    match tokens with
        | [] -> Error "Expected key, found end of input"
        | (JString s, _)::(KeyEnd, _)::xs -> Ok(JSONKey s, xs)
        | (invalid, meta)::_ -> (getPosition meta + " - Expected key, found " + string invalid ) |> Error
    )

let rec private parseJSON () =
    let parser = parse {
        return! parseLiteral
        return! parseArray () // hack around F#'s strict evaluation by defining these parsers as expr8y functions
        return! parseObject ()
    }
    parser

and private parseArray () =
    let parser = parse {
        do! parseArrayStart
        let parseJSONObjWithComma = parse {
            let! value = parseJSON ()
            do! parseComma
            return value
        }
        let! jsons = zeroOrMore parseJSONObjWithComma
        if List.isEmpty jsons then
            let! maybeOneEntry = maybeOne (parseJSON())
            do! parseArrayEnd
            return Array maybeOneEntry
        else
            let! lastEntry = parseJSON()
            do! parseArrayEnd
            return List.append jsons [lastEntry] |> Array
    }
    parser

and private parseObject () =
    let parser = parse {
        do! parseObjectStart
        let parseKVPair = parse {
            let! key = parseKey
            let! value = parseJSON ()
            return (key, value)
        }
        let parseKVPairWithComma = parse {
            let! key = parseKey
            let! value = parseJSON ()
            do! parseComma
            return (key, value)
        }
        let! kvPairs = zeroOrMore parseKVPairWithComma
        if List.isEmpty kvPairs then
            let! maybeOneEntry = maybeOne parseKVPair
            do! parseObjectEnd
            return Map.ofList maybeOneEntry |> Object
        else
            let! lastEntry = parseKVPair
            do! parseObjectEnd
            return List.append kvPairs [lastEntry] |> Map.ofList |> Object
    }
    parser

let run (Parser func) = func >> (fun result ->
    match result with
        | Error e -> Error e
        | Ok (j, leftover) -> if List.isEmpty leftover then Ok j else let t = List.head leftover |> string in "Unexpected token " + t |> Error
    )

let jsonParse input =
    match tokenize input with
        | Ok tokens -> run (parseJSON()) tokens
        | Error (TokenizeError e) -> Error e

let rec jsonSerialize json =
    match json with
        | String s -> "\"" + s + "\""
        | Number n -> string(n)
        | Null -> "null"
        | Boolean b -> string(b)
        | Array a -> "[" + (List.map jsonSerialize a |> String.concat ",") + "]"
        | Object o -> "{" + ((Map.foldBack (fun (JSONKey k) v a -> "\"" + k + "\":" + jsonSerialize v::a) o List.empty) |> String.concat ",") + "}"