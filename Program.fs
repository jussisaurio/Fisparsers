﻿open System
open JSONParser
open CGenerate

let always v _ = v

let tee f x =
    f x
    x

let testJSONinput = "    [{\"_id\":\"5f82451d6e90359a84e29068\", \"index\":0,\"guid\":\"18aca3fe-d348-4fe4-b44e-3c65ad074164\",\"isActive\":false,\"balance\":\"$2,191.23\",\"picture\":\"http://placehold.it/32x32\",\"age\":29,\"eyeColor\":\"blue\",\"name\":\"Carla Lloyd\",\"gender\":\"female\",\"company\":\"FUTURIZE\",\"email\":\"carlalloyd@futurize.com\",\"phone\":\"+1 (987) 544-3958\",\"address\":\"744 Losee Terrace, Belfair, Massachusetts, 8268\",\"about\":\"Veniam ex officia voluptate mollit labore ea. Qui elit enim eu cupidatat in fugiat exercitation velit ad deserunt amet commodo do. Laboris est ipsum et reprehenderit eu magna excepteur. Qui culpa eiusmod magna nisi velit dolor proident esse voluptate veniam exercitation ipsum mollit.\\r\\n\",\"registered\":\"2015-07-19T09:15:40 -03:00\",\"latitude\":-43.917934,\"longitude\":-6.960119,\"tags\":[\"eiusmod\",\"enim\",\"consectetur\",\"quis\",\"fugiat\",\"esse\",\"irure\"],\"friends\":[{\"id\":0,\"name\":\"Ora Wilkins\"},{\"id\":1,\"name\":\"Laurel Dean\"},{\"id\":2,\"name\":\"Hayes Cleveland\"}],\"greeting\":\"Hello, Carla Lloyd! You have 5 unread messages.\",\"favoriteFruit\":\"strawberry\"}]"
let json args =
    let input = Array.tryHead args |> Option.defaultValue testJSONinput
    match jsonParse input with
    | Ok output -> jsonSerialize output |> Console.WriteLine |> always 0
    | Error e -> "Parse failed: " + e |> Console.WriteLine |> always 1

let compileC (file: string) =
    let outFile = file.Replace(".c", ".s")
    let src = IO.File.ReadAllText file
    match compileCProgram src with
    | Ok output -> IO.File.WriteAllText (outFile, output) |> always (Ok (outFile))
    | Error (ParseError e) -> "C parsing failed: " + e |> tee Console.WriteLine |> (fun e -> Error (file, e))
    | Error (CodeGenerationError e) -> "C code generation failed: " + e |> tee Console.WriteLine |> (fun e -> Error (file, e))

let c args =
    if Array.isEmpty args then "Provide at least one input file" |> Console.WriteLine |> always 1
    else
        let printErrors result =
            match result with
            | Ok f -> ignore f
            | Error (f, e) -> "Error compiling " + f + ": " + e |> Console.WriteLine
        let results = Array.map compileC args
        Array.map printErrors results |> ignore
        if Array.exists (fun r ->
            match r with
            | Error _ -> true
            | Ok _ -> false
        ) results then 1 else 0



[<EntryPoint>]
let main argv =
    let program = Array.tryHead argv |> Option.defaultValue "json"
    let args = if Array.length argv > 1 then argv.[1..] else Array.empty
    match program with
        | "json" -> json args
        | "c" -> c args
        | _ -> Console.WriteLine "Provide either 'json' or 'c' as the first argument" |> always 1

