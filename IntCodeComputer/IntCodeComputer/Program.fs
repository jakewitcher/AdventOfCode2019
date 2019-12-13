// Learn more about F# at http://fsharp.org

open System
open System.IO
open IntCodeComputer.Types
open IntCodeComputer.IO
open Helpers

[<EntryPoint>]
let main argv =
    Console.WriteLine "Enter Input Instruction: [1] - Air Conditioner Unit; [5] - Thermal Radiator Controler "
    let input = Console.ReadLine() |> string |> tryParseInt

    match input with
    | Some input' -> runInputInstruction input' |> writeResultsToConsole
    | None -> printf "The Input Instruction provided was not valid"

    0 // return an integer exit code
