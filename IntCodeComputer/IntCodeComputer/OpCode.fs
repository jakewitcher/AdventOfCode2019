module OpCode

open IntCodeComputer.Types
open Collections

let rec private pad length instruction =
    if List.length instruction = length then instruction
    else pad length (0 :: instruction)

let padThree = pad 3
let padFour = pad 4

let private parseMode mode =
    match mode with
    | 0 -> Ok Position
    | 1 -> Ok Immediate
    | mode -> sprintf "%i is not a valid parameter mode" mode |> Error


module Create =
    let private createInput mode input =
        parseMode mode
        |> Result.map (fun mode ->
            { InputParameter = input
              Mode = mode })

    let private createOutput mode output =
        parseMode mode
        |> Result.map (fun mode ->
            { OutputParameter = output
              Mode = mode })

    let private createThreeParameterOpCode ctor rawInstruction rawInput1 rawInput2 rawOutput =
        let instruction = padFour rawInstruction

        let input1 = createInput instruction.[1] rawInput1
        let input2 = createInput instruction.[0] rawInput2
        let output = createOutput 0 rawOutput

        match input1, input2, output with
        | Ok in1, Ok in2, Ok out -> ctor (in1, in2, out) |> Ok
        | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

    let private createTwoParamOpCode ctor rawInstruction rawInput1 rawInput2 =
        let instruction = padFour rawInstruction
    
        let input1 = createInput instruction.[1] rawInput1
        let input2 = createInput instruction.[0] rawInput2

        match input1, input2 with
        | Ok in1, Ok in2 -> ctor (in1, in2) |> Ok
        | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

    let private createOne = createThreeParameterOpCode One
    let private createTwo = createThreeParameterOpCode Two

    let private createThree rawInstruction rawInput rawOutput =
        let instruction = padThree rawInstruction

        let input = createInput instruction.[0] rawInput
        let output = createOutput 0 rawOutput

        match input, output with
        | Ok in1, Ok out -> Three(in1, out) |> Ok
        | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

    let private createFour rawInstruction rawOutput =
        let instruction = padThree rawInstruction

        let output = createOutput instruction.[0] rawOutput 

        match output with
        | Ok out -> Four(out) |> Ok
        | _ -> "one or more parameter modes was invalid" |> (Error << InvalidParameterMode)

    let private createFive = createTwoParamOpCode Five
    let private createSix = createTwoParamOpCode Six
    let private createSeven = createThreeParameterOpCode Seven
    let private createEight = createThreeParameterOpCode Eight

    let opcode instruction state =
        let createOpCode opt f =
            opt
            |> Option.map f
            |> Option.defaultValue
                ("not enough parameters for opcode one"
                    |> MissingParameters
                    |> Error)
    
        let singleParameter = 
            createOpCode (Array.tryGet (state.Position + 1) state.Program)

        let multiParameter length = 
            createOpCode (Array.trySub state.Program (state.Position + 1) length)

        let extractTwoParameters f (param : int []) = 
            f instruction param.[0] param.[1]

        let extractThreeParameters f (param : int []) = 
            f instruction param.[0] param.[1] param.[2]
        
        match List.tryLast instruction with
        | Some 1 -> extractThreeParameters createOne |> multiParameter 3
        | Some 2 -> extractThreeParameters createTwo |> multiParameter 3
        | Some 3 -> createThree instruction state.UserInput |> singleParameter
        | Some 4 -> createFour instruction |> singleParameter
        | Some 5 -> extractTwoParameters createFive |> multiParameter 2
        | Some 6 -> extractTwoParameters createSix |> multiParameter 2
        | Some 7 -> extractThreeParameters createSeven |> multiParameter 3
        | Some 8 -> extractThreeParameters createEight |> multiParameter 3
        | Some 99 -> Ok NinetyNine
        | Some opcode -> sprintf "%i is not a valid opcode" opcode |> (Error << InvalidOpCode)
        | None -> "no opcode found" |> (Error << MissingOpCode)


module Read =
    let private processInput (input: Input) program =
        match input.Mode with
        | Position -> Array.tryGet input.InputParameter program
        | Immediate -> Some input.InputParameter

    let private processOutput (output: Output) program =
        match output.Mode with
        | Position -> Array.tryGet output.OutputParameter program
        | Immediate -> Some output.OutputParameter

    let private readThreeParameterOpCode f unprocessedInput1 unprocessedInput2 output state =
        let input1 = processInput unprocessedInput1 state.Program
        let input2 = processInput unprocessedInput2 state.Program

        match input1, input2 with
        | Some in1, Some in2 ->
            { state with
                    Position = state.Position + 4
                    Program = Array.updateAt output.OutputParameter (f in1 in2) state.Program }
            |> Ok
        | _, _ ->
            "invalid parameter" |> (Error << InvalidParameter)

    let private readConditionalJumpOpCode f unprocessedInput1 unprocessedOnput2 state =
        let input1 = processInput unprocessedInput1 state.Program
        let input2 = processInput unprocessedOnput2 state.Program

        match input1, input2 with
        | Some in1, Some in2 ->
            { state with
                    Position = if f in1 0 then in2 else state.Position + 3 }
            |> Ok
        | _, _ ->
            "invalid parameter" |> (Error << InvalidParameter)

    let private readComparisonOpCode f unprocessedInput1 unprocessedInput2 outputPosition state =
        let input1 = processInput unprocessedInput1 state.Program
        let input2 = processInput unprocessedInput2 state.Program

        match input1, input2 with
        | Some in1, Some in2 ->
            let output = if f in1 in2 then 1 else 0
            { state with
                    Position = state.Position + 4
                    Program = Array.updateAt outputPosition output state.Program }
            |> Ok
        | _, _ ->
            "invalid parameter" |> (Error << InvalidParameter)

    let private readOne = readThreeParameterOpCode (+)
    let private readTwo = readThreeParameterOpCode (*)

    let private readThree input output state =
        { state with
            Position = state.Position + 2
            Program = Array.updateAt output.OutputParameter input.InputParameter state.Program } |> Ok

    let private readFour unprocessedOutput state =
        let output = processOutput unprocessedOutput state.Program
        match output with 
        | Some out -> 
            { state with
                    Position = state.Position + 2
                    Output = out :: state.Output } |> Ok
        | None -> 
            "invalid parameter" |> (Error << InvalidParameter)

    let private readFive = readConditionalJumpOpCode (<>)
    let private readSix = readConditionalJumpOpCode (=)
    let private readSeven = readComparisonOpCode (<)
    let private readEight = readComparisonOpCode (=)

    let opcode op state =
        match op with
        | One(in1, in2, out) -> readOne in1 in2 out state
        | Two(in1, in2, out) -> readTwo in1 in2 out state
        | Three(in1, out) -> readThree in1 out state
        | Four out -> readFour out state
        | Five(in1, in2) -> readFive in1 in2 state
        | Six(in1, in2) -> readSix in1 in2 state
        | Seven(in1, in2, out) -> readSeven in1 in2 out.OutputParameter state
        | Eight(in1, in2, out) -> readEight in1 in2 out.OutputParameter state
        | NinetyNine -> { state with Status = Halt } |> Ok


module Run =
    let rec private segment (instruction: int) (segments: int list) =
        if instruction <= 0 then segments
        else segment (instruction / 10) (instruction % 10 :: segments)

    let opcode (state: ProgramState) =

        let segmented =
            if state.Program.[state.Position] = 99 then [99] 
            else segment state.Program.[state.Position] []

        if (not << List.isEmpty) segmented then
            Create.opcode segmented state |> Result.bind (fun opcode -> Read.opcode opcode state)
        else
            sprintf "%i is not a valid opcode" state.Program.[state.Position]
            |> (Error << InvalidOpCode)