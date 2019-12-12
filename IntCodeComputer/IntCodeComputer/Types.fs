namespace IntCodeComputer.Types

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
    | Five of Input * Input
    | Six of Input * Input
    | Seven of Input * Input * Output
    | Eight of Input * Input * Output
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
    | InvalidParameterMode of string
    | InvalidParameter of string
    | MissingParameters of string
    | InvalidOpCode of string
    | MissingOpCode of string