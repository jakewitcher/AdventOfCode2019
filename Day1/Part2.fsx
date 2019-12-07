open System.IO

let flip f x y = f y x

let parseModuleFuelAmounts (amounts: string array) =
    let tryParseFloat (str: string) =
        try
            Some(float <| str.Trim())
        with _ -> None

    amounts
    |> Array.toList
    |> List.choose tryParseFloat

let calculateFuelRequirement (amount: float) =
    let calculate' (a: float) =
        a / 3.
        |> floor
        |> flip (-) 2.

    let rec calculate (current: float, total: float) =
        let newAmount = calculate' current
        if newAmount <= 0. then total
        else calculate (newAmount, newAmount + total)

    calculate (amount, 0.)

let calculateModuleFuelRequirements (amounts: float list) = amounts |> List.sumBy calculateFuelRequirement

let getModuleFuelRequirements = parseModuleFuelAmounts >> calculateModuleFuelRequirements

let fuelAmounts =
    sprintf "%s\\fuel.txt" __SOURCE_DIRECTORY__
    |> File.ReadAllLines
    |> getModuleFuelRequirements
    |> int
 