module OpCode

open IntCodeComputer.Types
open Collections

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

let private createInput (mode: int) (input: Parameter) =
    parseMode mode
    |> Result.map (fun mode ->
        { InputParameter = input
          Mode = mode })

let private createOutput (mode: int) (output: Parameter) =
    parseMode mode
    |> Result.map (fun mode ->
        { OutputParameter = output
          Mode = mode })

let private createThreeParamOpCode (ctor: Input * Input * Output -> OpCode) (instruction: int list) (input1: Parameter) (input2: Parameter) (output: Parameter) =
    let instruction' = padFour instruction

    let input1' = createInput instruction'.[1] input1
    let input2' = createInput instruction'.[0] input2
    let output' = createOutput 0 output

    match input1', input2', output' with
    | Ok in1, Ok in2, Ok out -> ctor (in1, in2, out) |> Ok
    | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

let private createTwoParamOpCode (ctor: Input * Input -> OpCode) (instruction: int list) (input1: Parameter) (input2: Parameter) =
    let instruction' = padFour instruction
    
    let input1' = createInput instruction'.[1] input1
    let input2' = createInput instruction'.[0] input2

    match input1', input2' with
    | Ok in1, Ok in2 -> ctor (in1, in2) |> Ok
    | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

let createOne = createThreeParamOpCode One
let createTwo = createThreeParamOpCode Two

let createThree (instruction: int list) (input: Parameter) (output: Parameter) =
    let instruction' = padThree instruction

    let input' = createInput instruction'.[0] input
    let output' = createOutput 0 output

    match input', output' with
    | Ok in1, Ok out -> Three(in1, out) |> Ok
    | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

let createFour (instruction: int list) (output: Parameter): Result<OpCode, IntCodeError> =
    let instruction' = padThree instruction

    let output' = createOutput instruction'.[0] output 

    match output' with
    | Ok out -> Four(out) |> Ok
    | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

let createFive = createTwoParamOpCode Five
let createSix = createTwoParamOpCode Six
let createSeven = createThreeParamOpCode Seven
let createEight = createThreeParamOpCode Eight

let create (instruction: int list) (state: ProgramState) =
    let createOpCode opt f =
        opt
        |> Option.map f
        |> Option.defaultValue
            ("not enough parameters for opcode one"
                |> MissingParameters
                |> Error)
    
    let createOpCodeSingleParam = createOpCode (Array.tryGet (state.Position + 1) state.Program)
    let createOpCodeMultiParam length = createOpCode (Array.trySub state.Program (state.Position + 1) length)
    let twoParam f (param : int []) = f instruction param.[0] param.[1]
    let threeParam f (param : int []) = f instruction param.[0] param.[1] param.[2]
        
    match List.tryLast instruction with
    | Some 1 -> createOpCodeMultiParam 3 (threeParam createOne)
    | Some 2 -> createOpCodeMultiParam 3 (threeParam createTwo)
    | Some 3 -> createOpCodeSingleParam (fun param -> createThree instruction state.UserInput param)
    | Some 4 -> createOpCodeSingleParam (fun param -> createFour instruction param)
    | Some 5 -> createOpCodeMultiParam 2 (twoParam createFive)
    | Some 6 -> createOpCodeMultiParam 2 (twoParam createSix)
    | Some 7 -> createOpCodeMultiParam 3 (threeParam createSeven)
    | Some 8 -> createOpCodeMultiParam 3 (threeParam createEight)
    | Some 99 -> Ok NinetyNine
    | Some opcode ->
        sprintf "%i is not a valid opcode" opcode |> (Error << InvalidOpCode)
    | None ->
        "no opcode found" |> (Error << MissingOpCode)

let processInput (input: Input) (program: Program) =
    match input.Mode with
    | Position -> Array.tryGet input.InputParameter program
    | Immediate -> Some input.InputParameter

let processOutput (output: Output) (program: Program) =
    match output.Mode with
    | Position -> Array.tryGet output.OutputParameter program
    | Immediate -> Some output.OutputParameter

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
            "invalid parameter" |> (Error << InvalidParameter)


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
            "invalid parameter" |> (Error << InvalidParameter)

    | Three(in1, out) ->
        { state with
                Position = state.Position + 2
                Program = Array.updateAt out.OutputParameter in1.InputParameter state.Program }
        |> Ok

    | Four out ->
        let output = processOutput out state.Program
        match output with 
        | Some output' -> 
            { state with
                    Position = state.Position + 2
                    Output = output' :: state.Output } |> Ok
        | None -> 
            "invalid parameter" |> (Error << InvalidParameter)

    | Five(in1, in2) ->
        let param1 = processInput in1 state.Program
        let param2 = processInput in2 state.Program

        match param1, param2 with
        | Some in1', Some in2' ->
            { state with
                    Position = if in1' <> 0 then in2' else state.Position + 3 }
            |> Ok
        | _, _ ->
            "invalid parameter" |> (Error << InvalidParameter)

    | Six(in1, in2) ->
        let param1 = processInput in1 state.Program
        let param2 = processInput in2 state.Program

        match param1, param2 with
        | Some in1', Some in2' ->
            { state with
                    Position = if in1' = 0 then in2' else state.Position + 3 }
            |> Ok
        | _, _ ->
            "invalid parameter" |> (Error << InvalidParameter)

    | Seven(in1, in2, out) ->
        let param1 = processInput in1 state.Program
        let param2 = processInput in2 state.Program

        match param1, param2 with
        | Some in1', Some in2' ->
            let output = if in1' < in2' then 1 else 0
            { state with
                    Position = state.Position + 4
                    Program = Array.updateAt out.OutputParameter output state.Program }
            |> Ok
        | _, _ ->
            "invalid parameter" |> (Error << InvalidParameter)

    | Eight(in1, in2, out) ->
        let param1 = processInput in1 state.Program
        let param2 = processInput in2 state.Program

        match param1, param2 with
        | Some in1', Some in2' ->
            let output = if in1' = in2' then 1 else 0
            { state with
                    Position = state.Position + 4
                    Program = Array.updateAt out.OutputParameter output state.Program }
            |> Ok
        | _, _ ->
            "invalid parameter" |> (Error << InvalidParameter)

    | NinetyNine -> { state with Status = Halt } |> Ok

let rec private segment (instruction: int) (segments: int list) =
    if instruction <= 0 then segments
    else segment (instruction / 10) (instruction % 10 :: segments)

let read (state: ProgramState) =

    let segmented =
        if state.Program.[state.Position] = 99 then [99] 
        else segment state.Program.[state.Position] []

    if (not << List.isEmpty) segmented then
        create segmented state |> Result.bind (fun opcode -> run opcode state)
    else
        sprintf "%i is not a valid opcode" state.Program.[state.Position]
        |> (Error << InvalidOpCode)