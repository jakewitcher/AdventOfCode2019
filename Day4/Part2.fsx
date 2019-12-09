let passwordRange = [ 273025 .. 767253 ]

module Password =
    let toDigitList (password: int) =
        let rec toDigitList' password digits =
            if password <= 0 then digits
            else toDigitList' (password / 10) (password % 10 :: digits)

        toDigitList' password []

    let ofDigitList (digitList: int list) =
        digitList
        |> List.rev
        |> List.mapi (fun i elem -> elem * pown 10 i)
        |> List.sum

    let isSixDigits (password: int list) =
        if password.Length = 6 then Ok password
        else Error "password must have 6 digits"

    let isAscendingSeq (password: int list) =
        if password
           |> Seq.windowed 2
           |> Seq.forall (fun elem -> elem.[0] <= elem.[1])
        then Ok password
        else Error "password digits must ascend from left to right"

    let hasSameAdjacent (password: int list) =
        let isOnlyTwo (elem: int) (password: int list) =
            password
            |> List.filter (fun x -> x = elem)
            |> List.length = 2

        if password
           |> Seq.windowed 2
           |> Seq.exists (fun elem -> elem.[0] = elem.[1] && isOnlyTwo elem.[0] password)
        then Ok password
        else Error "password must have at least one pair of matching adjacent digits"

    let validatePassword (password: int) =
        password
        |> toDigitList
        |> isSixDigits
        |> Result.bind isAscendingSeq
        |> Result.bind hasSameAdjacent
        |> Result.map ofDigitList

    let findAllValid (unvalidated: int list) =
        unvalidated
        |> List.map validatePassword
        |> List.filter (function
            | Ok password -> true
            | Error _ -> false)

let totalValidPasswords =
    passwordRange
    |> Password.findAllValid
    |> List.length
