open System.IO

let tryParseInt (str: string) =
    try
        Some(int <| str.Trim())
    with _ -> None

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

module Op =
    type Operation =
        | One of int * int * int
        | Two of int * int * int
        | NinetyNine

    let create (op: int []) =
        match op.Length = 4, op.[0] with
        | true, 1 -> One(op.[1], op.[2], op.[3]) |> Some
        | true, 2 -> Two(op.[1], op.[2], op.[3]) |> Some
        | _, 99 -> NinetyNine |> Some
        | _ -> None

    let opcode (f: int -> int -> int) (i1: int) (i2: int) (out: int) (program: int []) =
        let input1 = Array.tryGet i1 program
        let input2 = Array.tryGet i2 program

        let output =
            match input1, input2 with
            | Some x, Some y -> Some(f x y)
            | _ -> None

        output |> Option.map (fun output -> Array.updateAt out output program)


    let opcode1 = opcode (+)
    let opcode2 = opcode (*)

    let run (prog: int []) (op: Operation option) =
        let run' prog op =
            match op with
            | One(i1, i2, out) -> opcode1 i1 i2 out prog
            | Two(i1, i2, out) -> opcode2 i1 i2 out prog
            | NinetyNine -> Some prog

        op |> Option.bind (run' prog)

module IntCodeProgram =
    let parse (file: string) = file.Split [| ',' |] |> Array.choose tryParseInt

    let initialize (noun: int) (verb: int) (program: int []) =
        program
        |> Array.updateAt 1 noun
        |> Array.updateAt 2 verb

    let run (program: int []) =
        let rec run (i: int, prog: int [] option) =
            let opSeg = prog |> Option.bind (fun prog -> Array.trySub prog i 4)

            match opSeg, prog with
            | Some seg, Some prog -> run (i + 4, Op.run prog (Op.create seg))
            | _ -> prog

        run (0, Some program)

    let initialState =
        sprintf "%s\\program.txt" __SOURCE_DIRECTORY__
        |> File.ReadAllText
        |> parse

module GravityAssist =
    open IntCodeProgram

    let run (noun: int) (verb: int) =
        initialState
        |> initialize noun verb
        |> run
        |> Option.map Array.head

    let findNounVerb =
        let inputs =
            Array.init 100 (fun _ -> [| 0 .. 99 |])
            |> Array.mapi (fun first inputs' -> Array.map (fun second -> (first, second)) inputs')
            |> Array.concat

        let rec findNounVerb' program =
            match program with
            | [] -> None
            | (noun, verb) :: _ when run noun verb = Some 19690720 -> Some(noun, verb)
            | _ :: tail -> findNounVerb' tail

        List.ofArray inputs
        |> findNounVerb'
        |> Option.map (fun (noun, verb) -> 100 * noun + verb)

        