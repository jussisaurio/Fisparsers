module Parser

type ParseError = string
type Parser<'T,'K> = Parser of ('K list -> Result<'T * 'K list, ParseError>)

let (>>=) result chainResult = Result.bind chainResult result

let private bind next (Parser parseFunc1) = Parser (fun inputList ->
    parseFunc1 inputList >>= (fun (value, rest) -> let (Parser parseFunc2) = next value in parseFunc2 rest)
    )

type ParserBuilder() =
  member x.Return(v) = Parser (fun inputList -> Ok(v, inputList))
  member x.ReturnFrom(m) = m
  member x.Bind(v, f) = bind f v
  member x.Zero() = Parser (fun _ -> Error "zero")
  // Use .Combine() to allow 'Alternative', i.e. terminate after first if Ok, try second if Error
  member x.Combine((Parser parseFunc1), (Parser parseFunc2))
    = Parser (fun inputList ->
        match parseFunc1 inputList with
            | Ok parsed -> Ok parsed
            | Error _ -> parseFunc2 inputList
    )
  member x.Delay(f) = Parser (fun inputList -> let (Parser parseFunc) = f () in parseFunc inputList)

let parse = ParserBuilder()

let rec oneOrMore p = parse {
  let! x = p
  let! xs = zeroOrMore p
  return x::xs }

and zeroOrMore p = parse {
  return! oneOrMore p
  return [] }

let maybeOne p = parse {
    return! parse {
        let! x = p
        return [x]
    }
    return []
}