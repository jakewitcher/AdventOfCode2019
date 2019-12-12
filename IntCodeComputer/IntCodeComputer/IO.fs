module IntCodeComputer.IO

open System.IO
open IntCodeComputer.Types

let runInputInstruction input =
    sprintf "%s\\Data\\program.txt" __SOURCE_DIRECTORY__
    |> File.ReadAllText
    |> Computer.parseProgram
    |> Computer.run input

let private writeIntCodeComputerResults method result =
    match result with 
    | Ok state -> sprintf "%A" state.Output |> method
    | Error err -> 
        match err with 
        | InvalidParameterMode msg -> sprintf "%A" msg |> method
        | InvalidParameter msg -> msg |> method 
        | MissingParameters msg -> msg |> method
        | InvalidOpCode msg -> msg |> method 
        | MissingOpCode msg -> msg |> method

let writeToConsole = writeIntCodeComputerResults (printfn "%s")

let writeToFile path = writeIntCodeComputerResults (fun result -> File.WriteAllText(path, result))