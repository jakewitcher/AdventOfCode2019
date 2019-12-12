module Helpers

let tryParseInt (str: string) =
    try
        Some(int <| str.Trim())
    with _ -> None