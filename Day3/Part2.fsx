open System.IO

let tryParseInt (str: string) =
    try
        Some(int <| str.Trim())
    with _ -> None

type Point = int * int

type Line =
    { PointA: Point
      PointB: Point
      Length: float
      WireLength: float }

type IntersectionPoint =
    { Point: Point
      WireALength: float
      WireBLength: float }

type Direction =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

module Direction =
    let tryParse (direction: string) =
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

    let tryParseAll (file: string) =
        file.Split [| ',' |]
        |> Array.choose tryParse
        |> List.ofArray

module Point =
    let ofDirection (directions: Direction list): Point list =
        let createPoint ((x, y), points) direction =
            match direction with
            | Up a ->
                let newPoint = (x, y + a)
                newPoint, newPoint :: points
            | Down b ->
                let newPoint = (x, y - b)
                newPoint, newPoint :: points
            | Left c ->
                let newPoint = (x - c, y)
                newPoint, newPoint :: points
            | Right d ->
                let newPoint = (x + d, y)
                newPoint, newPoint :: points

        directions
        |> List.fold createPoint ((0, 0), [])
        |> snd

    let distance (pointA: Point) (pointB: Point) =
        pown (fst pointA - fst pointB) 2 + pown (snd pointA - snd pointB) 2
        |> float
        |> sqrt


module Line =
    let create (pointA: Point) (pointB: Point) =
        let length = Point.distance pointA pointB
        { PointA = pointA
          PointB = pointB
          Length = length
          WireLength = 0. }

    let isHorizontal (line: Line) =
        let _, y = line.PointA
        let _, y' = line.PointB

        line.Length <> 0. && y - y' = 0

    let isVertical (line: Line) =
        let x, _ = line.PointA
        let x', _ = line.PointB

        line.Length <> 0. && x - x' = 0

    let private tryFindIntersection (horizontal: Line) (vertical: Line): IntersectionPoint option =
        let isBetween (a: int) (b: int) (c: int) = c < max a b && c > min a b

        let vxa, vya = vertical.PointA
        let vxb, vyb = vertical.PointB
        let hxa, hya = horizontal.PointA
        let hxb, hyb = horizontal.PointB

        if (isBetween hxa hxb vxa) && (isBetween vya vyb hya) then
            { Point = (vxa, hya)
              WireALength = vertical.WireLength - (Point.distance vertical.PointB (vxa, hya))
              WireBLength = horizontal.WireLength - (Point.distance horizontal.PointB (vxa, hya)) }
            |> Some
        else
            None

    let intersection (line1: Line) (line2: Line): IntersectionPoint option =
        match isHorizontal line1, isVertical line2 with
        | true, true -> tryFindIntersection line1 line2
        | false, false -> tryFindIntersection line2 line1
        | _ -> None

module Intersections =
    let findAll (wire1: seq<Line>) (wire2: seq<Line>) =
        wire1 |> Seq.collect (fun wire1' -> Seq.choose (fun wire2' -> Line.intersection wire1' wire2') wire2)

    let findShortestDistance (wire1: seq<Line>) (wire2: seq<Line>) =

        findAll wire1 wire2
        |> Seq.map (fun intersection -> intersection.WireALength + intersection.WireBLength)
        |> Seq.min

module FuelManagementSystem =
    let wire1, wire2 =
        sprintf "%s\\wires.txt" __SOURCE_DIRECTORY__
        |> File.ReadAllLines
        |> Array.map
            (Direction.tryParseAll
             >> Point.ofDirection
             >> List.rev
             >> Seq.windowed 2
             >> Seq.map (fun points -> Line.create points.[0] points.[1])
             >> Seq.fold
                 (fun state line ->
                 (fst state + line.Length, { line with WireLength = line.Length + fst state } :: (snd state))) (0., [])
             >> snd
             >> List.rev)
        |> (fun wires -> wires.[0], wires.[1])

    let shortestDistanceIntersection = Intersections.findShortestDistance wire1 wire2
