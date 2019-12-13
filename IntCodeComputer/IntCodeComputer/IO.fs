module IntCodeComputer.IO

open System.IO
open IntCodeComputer.Types

let runInputInstruction input =
    sprintf "%s\\Data\\program.txt" __SOURCE_DIRECTORY__
    |> File.ReadAllText
    |> Computer.parseProgram
    |> Computer.run input

let writeResultsToConsole result =
    match result with 
    | Ok state -> printfn "%A" state.Output
    | Error err -> 
        match err with 
        | InvalidParameterMode msg -> printfn "%s" msg
        | InvalidParameter msg -> printfn "%s" msg 
        | MissingParameters msg -> printfn "%s" msg
        | InvalidOpCode msg -> printfn "%s" msg
        | MissingOpCode msg -> printfn "%s" msg