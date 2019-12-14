open System.IO

type Object = string

type Orbit =
    | Object of Object
    | Center of Object * Orbit list

module List =
    let tryHead list =
        match list with
        | [] -> None
        | list' -> List.head list' |> Some

module Orbit =
    let parse (orbit: string) =
        let validateOrbit (orbit: string []) =
            if Array.length orbit = 2 then Some(orbit.[0], orbit.[1])
            else None

        orbit.Split [| ')' |] |> validateOrbit

    let parseAll =
        sprintf "%s\\orbits.txt" __SOURCE_DIRECTORY__
        |> File.ReadAllLines
        |> Array.choose parse
        |> Array.toList

    let findCenter orbits =
        orbits
        |> List.unzip
        |> fun (centers, orbits) -> Set.difference (Set.ofList centers) (Set.ofList orbits)
        |> Set.toList
        |> List.tryHead

    let rec buildOrbitTree orbitPairs object =
        let orbitsOfOrbittingObjects object =
            match object with
            | Object obj -> buildOrbitTree orbitPairs obj
            | Center(_, _) -> object

        let buildOrbits object orbits =
            match orbits with
            | [] -> object |> Object
            | orbits' -> (object, orbits' |> List.map orbitsOfOrbittingObjects) |> Center

        orbitPairs
        |> List.filter (fun orbit -> fst orbit = object)
        |> List.map (snd >> Object)
        |> buildOrbits object

    let findAll orbits =
        orbits
        |> findCenter
        |> Option.map (Ok << buildOrbitTree orbits)
        |> Option.defaultValue ("no orbits Found" |> Error)

    let length orbit =
        let rec length' total orbit =
            match orbit with
            | Object _ -> total
            | Center(_, orbits) -> total + List.sumBy (length' (total + 1)) orbits

        length' 0 orbit

    let pathTo object orbit =
        let rec pathTo' path object orbit =
            match orbit with
            | Object obj ->
                if obj = object then obj :: path
                else []
            | Center(center, orbits) ->
                if center = object then center :: path
                else orbits |> List.collect (pathTo' (center :: path) object)

        pathTo' [] object orbit |> List.rev

    let distance object1 object2 orbit =
        let lastCommonNode (path1, path2) =
            Set.intersect (Set.ofList path1) (Set.ofList path2)
            |> Set.toList
            |> List.length
            |> fun distance -> path1, path2, distance - 1

        let pathFromCommon (path1, path2, common) =
            let fromCommon path =
                path
                |> List.skip common
                |> Set.ofList

            fromCommon path1, fromCommon path2

        (orbit |> pathTo object1, orbit |> pathTo object2)
        |> (lastCommonNode >> pathFromCommon)
        ||> Set.union
        |> Set.toSeq
        |> Seq.windowed 2
        |> Seq.length


Orbit.parseAll
|> Orbit.findAll
|> Result.map Orbit.length

Orbit.parseAll
|> Orbit.findAll
|> Result.map (Orbit.distance "SAN" "YOU")
