open System.IO

let tryParseInt (str: string) =
    try
        Some(int <| str.Trim())
    with _ -> None

type Point = Point of x: int * y: int

type Direction =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

let parseDirection (direction: string) =
    let parse (str: string) (ctor: int -> Direction) =
        str.Substring(1)
        |> tryParseInt
        |> Option.map ctor

    match direction.Substring(0, 1) with
    | "U" -> parse direction Up
    | "D" -> parse direction Down
    | "L" -> parse direction Left
    | "R" -> parse direction Right
    | _ -> None

let recordPoints (directions: Direction list) =
    let createPoint (Point(x, y), points) direction =
        match direction with
        | Up a ->
            let newPoint = Point(x, y + a)
            newPoint, newPoint :: points
        | Down b ->
            let newPoint = Point(x, y - b)
            newPoint, newPoint :: points
        | Left c ->
            let newPoint = Point(x - c, y)
            newPoint, newPoint :: points
        | Right d ->
            let newPoint = Point(x + d, y)
            newPoint, newPoint :: points

    directions
    |> List.fold createPoint (Point(0, 0), [])
    |> snd

let parseWires (file: string) =
    file.Split [| ',' |]
    |> Array.choose parseDirection
    |> List.ofArray

let findIntersection (wire1: Point * Point) (wire2: Point * Point) =
    match wire1, wire2 with
    | (Point(a, b), Point(c, d)), (Point(a', b'), Point(c', d')) when a = c && b' = d' ->
        if (a >= a' && a <= c' || a <= a' && a >= c') && (b' >= b && b' <= d || b' <= b && b' >= d) then
            Point(a, b') |> Some
        else None
    | (Point(a, b), Point(c, d)), (Point(a', b'), Point(c', d')) when a' = c' && b = d ->
        if (a' >= a && a' <= c || a' <= a && a' >= c) && (b >= b' && b <= d' || b <= b' && b >= d') then
            Point(a', b) |> Some
        else None
    | _ -> None

let findAllIntersections (wire1: seq<Point * Point>) (wire2: seq<Point * Point>) =
    wire1 |> Seq.collect (fun wire1' -> Seq.choose (fun wire2' -> findIntersection wire1' wire2') wire2)

let closestIntersection =
    sprintf "%s\\wires.txt" __SOURCE_DIRECTORY__
    |> File.ReadAllLines
    |> Array.map
        (parseWires
         >> recordPoints
         >> Seq.windowed 2
         >> Seq.map (fun arr -> (arr.[0], arr.[1])))
    |> (fun arr -> findAllIntersections arr.[0] arr.[1])
    |> Seq.map (fun (Point(x, y)) -> abs x + abs y)
    |> Seq.min
