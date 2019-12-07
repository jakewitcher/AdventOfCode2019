open System.IO

let parseModuleFuelAmounts (amounts: string array) =
    let tryParseFloat (str: string) =
        try
            Some(float <| str.Trim())
        with _ -> None

    amounts
    |> Array.toList
    |> List.choose tryParseFloat

let calculateModuleFuelRequirements (amounts: float list) =
    let flip f x y = f y x

    amounts
    |> List.sumBy (fun amount ->
        amount / 3.
        |> floor
        |> flip (-) 2.)

let getModuleFuelRequirements = parseModuleFuelAmounts >> calculateModuleFuelRequirements

let fuelAmounts =
    sprintf "%s\\fuel.txt" __SOURCE_DIRECTORY__
    |> File.ReadAllLines
    |> getModuleFuelRequirements
    |> int
 