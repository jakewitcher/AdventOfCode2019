module Computer

open IntCodeComputer.Types
open Helpers
open OpCode

let parseProgram (file: string) = file.Split [| ',' |] |> Array.choose tryParseInt

let run input program =
    let state =
        { Program = program
          Position = 0
          UserInput = input
          Output = []
          Status = Run }

    let rec run' state =
        state
        |> Result.bind (fun progState ->
            if progState.Status = Halt || progState.Position >= Array.length progState.Program then state
            else Run.opcode progState |> run')

    run' (Ok state)

