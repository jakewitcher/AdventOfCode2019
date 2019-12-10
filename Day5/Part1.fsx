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
        | 0 -> Position
        | 1 -> Immediate
        | mode -> sprintf "%i is not a valid parameter mode" mode |> failwith

    let private twoParam (ctor: Input * Input * Output -> OpCode) (instruction: int list) (input1: Parameter)
        (input2: Parameter) (output: Parameter) =
        let instruction' = padFour instruction

        let input1' =
            { InputParameter = input1
              Mode = parseMode instruction'.[1] }

        let input2' =
            { InputParameter = input2
              Mode = parseMode instruction'.[0] }

        let output' =
            { OutputParameter = output
              Mode = Position }

        ctor (input1', input2', output')

    let createOne = twoParam One

    let createTwo = twoParam Two

    let createThree (instruction: int list) (input: Parameter) (output: Parameter) =
        let instruction' = padThree instruction

        let input' =
            { InputParameter = input
              Mode = parseMode instruction.[0] }

        let output' =
            { OutputParameter = output
              Mode = Position }

        Three(input', output')

    let createFour (instruction: int list) (output: Parameter) =
        let instruction' = padThree instruction

        let output' =
            { OutputParameter = output
              Mode = parseMode instruction.[0] }

        Four output'

    let create (instruction: int list) (state: ProgramState) =
        match List.tryLast instruction with
        | Some 1 ->
            Array.trySub state.Program (state.Position + 1) 3
            |> Option.map (fun param -> createOne instruction param.[0] param.[1] param.[2] |> Ok)
            |> Option.defaultValue (Error "not enough parameters for opcode one")

        | Some 2 ->
            Array.trySub state.Program (state.Position + 1) 3
            |> Option.map (fun param -> createTwo instruction param.[0] param.[1] param.[2] |> Ok)
            |> Option.defaultValue (Error "not enough parameters for opcode two")

        | Some 3 ->
            Array.tryGet (state.Position + 1) state.Program
            |> Option.map (fun param -> createThree instruction state.UserInput param |> Ok)
            |> Option.defaultValue (Error "no parameter exists for opcode three")

        | Some 4 ->
            Array.tryGet (state.Position + 1) state.Program
            |> Option.map (fun param -> createFour instruction param |> Ok)
            |> Option.defaultValue (Error "no parameter exists for opcode four")

        | Some opcode -> sprintf "%i is not a valid opcode" opcode |> Error

        | None -> "no opcode found" |> Error

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
            | _, _ -> "invalid parameter" |> Error

        | Two(in1, in2, out) ->
            let param1 = processInput in1 state.Program
            let param2 = processInput in2 state.Program

            match param1, param2 with
            | Some in1', Some in2' ->
                { state with
                      Position = state.Position + 4
                      Program = Array.updateAt out.OutputParameter (in1' * in2') state.Program }
                |> Ok
            | _, _ -> "invalid parameter" |> Error

        | Three(in1, out) ->
            let input = processInput in1 state.Program

            match input with
            | Some input' ->
                { state with
                      Position = state.Position + 2
                      Program = Array.updateAt out.OutputParameter input' state.Program }
                |> Ok
            | None -> "invalid Parameter" |> Error

        | Four out ->
            { state with
                  Position = state.Position + 2
                  Output = out.OutputParameter :: state.Output }
            |> Ok

        | NinetyNine -> { state with Status = Halt } |> Ok


module Opcode =

    let rec private segment (instruction: int) (segments: int list) =
        if instruction <= 0 then segments
        else segment (instruction / 10) (instruction % 10 :: segments)

    let read (state: ProgramState) =

        let segmented = segment state.Program.[state.Position] []

        if (not << List.isEmpty) segmented then
            OpCode.create segmented state |> Result.bind (fun opcode -> OpCode.run opcode state)
        else Error "no valid opcode was provided"


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

        let rec run' (state: Result<ProgramState, string>) =
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
