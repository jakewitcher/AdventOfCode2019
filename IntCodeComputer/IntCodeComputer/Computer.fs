module Computer

open IntCodeComputer.Types
open Helpers

let parseProgram (file: string) = file.Split [| ',' |] |> Array.choose tryParseInt

let run (input: int) (program: int []) =
    let state =
        { Program = program
          Position = 0
          UserInput = input
          Output = []
          Status = Run }

    let rec run' (state: Result<ProgramState, IntCodeError>) =
        state
        |> Result.bind (fun progState ->
            if progState.Status = Halt || progState.Position >= Array.length progState.Program then state
            else OpCode.read progState |> run')

    run' (Ok state)

