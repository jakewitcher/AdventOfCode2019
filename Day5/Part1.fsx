open System.IO

type Program = int []

type ProgramOutput = int list

type Pointer = int

type Parameter = int

type ParameterMode =
    | Position
    | Immediate

type Input =
    { Mode: ParameterMode
      InputParameter: Parameter }

type Output =
    { Mode: ParameterMode
      OutputParameter: Parameter }

type OpCode =
    | One of Input * Input * Output
    | Two of Input * Input * Output
    | Three of Input * Output
    | Four of Output
    | NinetyNine

type Status =
    | Run
    | Halt

type ProgramState =
    { Program: Program
      Position: Pointer
      UserInput: Parameter
      Output: ProgramOutput
      Status: Status }

type IntCodeError =
    | InvalidParameterMode of string list
    | InvalidParameter of string
    | MissingParameters of string
    | InvalidOpCode of string
    | MissingOpCode of string


module Array =
    let updateAt (index: int) (value: 't) (arr: 't []) =
        arr
        |> Array.mapi (fun i x ->
            if index = i then value
            else x)

    let tryGet (i: int) (arr: 't []) =
        if arr.Length > i then Some arr.[i]
        else None

    let trySub (arr: 't []) (i: int) (count: int) =
        try
            Array.sub arr i count |> Some
        with _ -> None


module List =
    let tryHead (list: 't list) =
        if List.isEmpty list then None
        else List.head list |> Some

    let tryLast (list: 't list) =
        if List.isEmpty list then None
        else List.last list |> Some


module OpCode =
    let rec private pad (length: int) (instruction: int list) =
        if List.length instruction = length then instruction
        else pad length (0 :: instruction)

    let padThree = pad 3

    let padFour = pad 4

    let private parseMode (mode: int) =
        match mode with
        | 0 -> Ok Position
        | 1 -> Ok Immediate
        | mode -> sprintf "%i is not a valid parameter mode" mode |> Error

    let private twoParam (ctor: Input * Input * Output -> OpCode) (instruction: int list) (input1: Parameter)
        (input2: Parameter) (output: Parameter) =
        let instruction' = padFour instruction

        let input1' =
            parseMode instruction'.[1]
            |> Result.map (fun mode ->
                { InputParameter = input1
                  Mode = mode })

        let input2' =
            parseMode instruction'.[0]
            |> Result.map (fun mode ->
                { InputParameter = input2
                  Mode = mode })

        let output' =
            { OutputParameter = output
              Mode = Position }

        match input1', input2' with
        | Ok in1, Ok in2 -> ctor (in1, in2, output') |> Ok
        | Error msg, Error msg' ->
            [ msg; msg' ]
            |> InvalidParameterMode
            |> Error
        | Error msg, _ ->
            [ msg ]
            |> InvalidParameterMode
            |> Error
        | _, Error msg ->
            [ msg ]
            |> InvalidParameterMode
            |> Error



    let createOne = twoParam One

    let createTwo = twoParam Two

    let createThree (instruction: int list) (input: Parameter) (output: Parameter) =
        let instruction' = padThree instruction

        let input' =
            parseMode instruction.[0]
            |> Result.map (fun mode ->
                { InputParameter = input
                  Mode = mode })

        let output' =
            { OutputParameter = output
              Mode = Position }

        match input' with
        | Ok in1 -> Three(in1, output') |> Ok
        | Error msg ->
            [ msg ]
            |> InvalidParameterMode
            |> Error


    let createFour (instruction: int list) (output: Parameter): Result<OpCode, IntCodeError> =
        let instruction' = padThree instruction

        let output' =
            { OutputParameter = output
              Mode = Position }

        Four output' |> Ok

    let create (instruction: int list) (state: ProgramState) =
        match List.tryLast instruction with
        | Some 1 ->
            Array.trySub state.Program (state.Position + 1) 3
            |> Option.map (fun param -> createOne instruction param.[0] param.[1] param.[2])
            |> Option.defaultValue
                ("not enough parameters for opcode one"
                 |> MissingParameters
                 |> Error)

        | Some 2 ->
            Array.trySub state.Program (state.Position + 1) 3
            |> Option.map (fun param -> createTwo instruction param.[0] param.[1] param.[2])
            |> Option.defaultValue
                ("not enough parameters for opcode two"
                 |> MissingParameters
                 |> Error)

        | Some 3 ->
            Array.tryGet (state.Position + 1) state.Program
            |> Option.map (fun param -> createThree instruction state.UserInput param)
            |> Option.defaultValue
                ("no parameter exists for opcode three"
                 |> MissingParameters
                 |> Error)

        | Some 4 ->
            Array.tryGet (state.Position + 1) state.Program
            |> Option.map (fun param -> createFour instruction param)
            |> Option.defaultValue
                ("no parameter exists for opcode four"
                 |> MissingParameters
                 |> Error)

        | Some opcode ->
            sprintf "%i is not a valid opcode" opcode
            |> InvalidOpCode
            |> Error

        | None ->
            "no opcode found"
            |> MissingOpCode
            |> Error

    let processInput (input: Input) (program: Program) =
        match input.Mode with
        | Position -> Array.tryGet input.InputParameter program
        | Immediate -> Some input.InputParameter

    let run (opcode: OpCode) (state: ProgramState) =
        match opcode with
        | One(in1, in2, out) ->
            let param1 = processInput in1 state.Program
            let param2 = processInput in2 state.Program

            match param1, param2 with
            | Some in1', Some in2' ->
                { state with
                      Position = state.Position + 4
                      Program = Array.updateAt out.OutputParameter (in1' + in2') state.Program }
                |> Ok
            | _, _ ->
                "invalid parameter"
                |> InvalidParameter
                |> Error

        | Two(in1, in2, out) ->
            let param1 = processInput in1 state.Program
            let param2 = processInput in2 state.Program

            match param1, param2 with
            | Some in1', Some in2' ->
                { state with
                      Position = state.Position + 4
                      Program = Array.updateAt out.OutputParameter (in1' * in2') state.Program }
                |> Ok
            | _, _ ->
                "invalid parameter"
                |> InvalidParameter
                |> Error

        | Three(in1, out) ->
            let input = processInput in1 state.Program

            match input with
            | Some input' ->
                { state with
                      Position = state.Position + 2
                      Program = Array.updateAt out.OutputParameter input' state.Program }
                |> Ok
            | None ->
                "invalid Parameter"
                |> InvalidParameter
                |> Error

        | Four out ->
            { state with
                  Position = state.Position + 2
                  Output = out.OutputParameter :: state.Output }
            |> Ok

        | NinetyNine -> { state with Status = Halt } |> Ok


module Opcode =

    let rec segment (instruction: int) (segments: int list) =
        if instruction <= 0 then segments
        else segment (instruction / 10) (instruction % 10 :: segments)

    let read (state: ProgramState) =

        let segmented = segment state.Program.[state.Position] []

        if (not << List.isEmpty) segmented then
            OpCode.create segmented state |> Result.bind (fun opcode -> OpCode.run opcode state)
        else
            sprintf "%i is not a valid opcode" state.Program.[state.Position]
            |> InvalidOpCode
            |> Error


module IntCodeComputer =
    let tryParseInt (str: string) =
        try
            Some(int <| str.Trim())
        with _ -> None

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
                if progState.Status = Halt || progState.Position > Array.length progState.Program then state
                else Opcode.read progState |> run')

        run' (Ok state)

let finalState =
    sprintf "%s\\program.txt" __SOURCE_DIRECTORY__
    |> File.ReadAllText
    |> IntCodeComputer.parseProgram
    |> IntCodeComputer.run 1
