open System.IO

let tryParseInt (str: string) =
    try
        Some(int <| str.Trim())
    with _ -> None

type Point = Point of x: int * y: int

type WireLength = int

type Direction =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

type WireMarker =
    { Point: Point
      Length: WireLength }

module WirePath =
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

        let createWireMarker (length: WireLength) (point: Point) =
            { Point = point
              Length = length }
        let createMarker (length, Point(x, y), markers) direction =
            match direction with
            | Up a ->
                let newPoint = Point(x, y + a)
                let newLength = length + abs a
                newLength, newPoint, (createWireMarker newLength newPoint) :: markers
            | Down b ->
                let newPoint = Point(x, y - b)
                let newLength = length + abs b
                newLength, newPoint, (createWireMarker newLength newPoint) :: markers
            | Left c ->
                let newPoint = Point(x - c, y)
                let newLength = length + abs c
                newLength, newPoint, (createWireMarker newLength newPoint) :: markers
            | Right d ->
                let newPoint = Point(x + d, y)
                let newLength = length + abs d
                newLength, newPoint, (createWireMarker newLength newPoint) :: markers

        let _, _, markers = directions |> List.fold createMarker (0, Point(0, 0), [])
        markers

    let parse (file: string) =
        file.Split [| ',' |]
        |> Array.choose parseDirection
        |> List.ofArray

module Intersection =
    open WirePath

    let find (wire1: WireMarker * WireMarker) (wire2: WireMarker * WireMarker) =
        let point1, point2 = wire1
        let point1', point2' = wire2

        match (point1.Point, point2.Point), (point1'.Point, point2'.Point) with
        | (Point(a, b), Point(c, d)), (Point(a', b'), Point(c', d')) when a = c && b' = d' ->
            if (a >= a' && a <= c' || a <= a' && a >= c') && (b' >= b && b' <= d || b' <= b && b' >= d) then
                let length = point1.Length + point1'.Length + abs (b' - b) + abs (a - a')
                { Point = Point(a, b')
                  Length = length }
                |> Some
            else
                None

        | (Point(a, b), Point(c, d)), (Point(a', b'), Point(c', d')) when a' = c' && b = d ->
            if (a' >= a && a' <= c || a' <= a && a' >= c) && (b >= b' && b <= d' || b <= b' && b >= d') then
                let length = point1.Length + point1'.Length + abs (b' - b) + abs (a - a')
                { Point = Point(a', b)
                  Length = length }
                |> Some
            else
                None
        | _ -> None

    let findAll (wire1: seq<WireMarker * WireMarker>) (wire2: seq<WireMarker * WireMarker>) =
        wire1 |> Seq.collect (fun wire1' -> Seq.choose (fun wire2' -> find wire1' wire2') wire2)

    let shortestLengthntersection =
        sprintf "%s\\wires.txt" __SOURCE_DIRECTORY__
        |> File.ReadAllLines
        |> Array.map
            (parse
             >> recordPoints
             >> List.rev
             >> Seq.windowed 2
             >> Seq.map (fun arr -> (arr.[0], arr.[1])))
        |> (fun arr -> findAll arr.[0] arr.[1])
        |> Seq.map (fun wireMarker -> wireMarker.Length)
        |> Seq.min
