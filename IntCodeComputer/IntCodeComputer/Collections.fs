module Collections

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

