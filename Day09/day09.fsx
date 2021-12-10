open System
open System.IO

//let FName = "Day09/sample.txt"
let FName = "Day09/input.txt"




let findLows (map : int[][])=

    let mx = map.[0].Length
    let my = map.Length

    let getNeighbourIndices (x, y) =
        [| x-1, y; x, y-1; x+1, y; x, y+1|]
        |> Array.filter(fun (x,y) -> x > -1 && x < mx && y > -1 && y < my)


    [| 0 .. my-1 |]
    |> Array.collect(fun y -> [| 0 .. mx-1 |] |> Array.map(fun x-> x,y))
    |> Array.map(fun (x,y) -> (x,y), map.[y].[x], getNeighbourIndices (x,y) |> Array.map(fun (x,y) -> map.[y].[x]))
    |> Array.map(fun (c,i,a) -> c,i, i < (a |> Array.min))
    |> Array.filter (fun (_,_,b) -> b)
    |> Array.map (fun (c,_,_) -> c)


let findRiskLevel (map : int[][])=

    map 
    |> findLows
    |> Array.map(fun (x,y) -> map.[y].[x] + 1) 
    |> Array.sum


// part 1 (506)
File.ReadAllLines(FName)
|> Array.map(fun s -> s |> Seq.map(fun c -> -48 + int c) |> Seq.toArray)
|> findRiskLevel


// part 2 (931200)
let getBasin (map : int[][]) (x,y) =
    let mx = map.[0].Length
    let my = map.Length

    let rec loop found todo =
        let getNeighbourIndices (x, y) =
            [| x-1, y; x, y-1; x+1, y; x, y+1|]
            |> Array.filter(fun (x,y) -> x > -1 && x < mx && y > -1 && y < my)

        match todo with
        | [] -> found
        | h::t -> 
            let n = 
                getNeighbourIndices h
                |> Array.map(fun (x,y) -> (x,y), map.[y].[x])
                |> Array.filter(fun (_,v) -> v < 9)
                |> Array.map(fun (c,_) -> c)
                |> Array.except found
                |> Array.toList
        
            loop (found @ n) (n @ t)

    loop [(x,y)] [(x,y)]

let findRiskArea (map : int[][]) =
    map
    |> findLows
    |> Array.map(fun c -> getBasin map c |> List.length |> int64)
    |> Array.sortDescending
    |> Array.take 3
    |> Array.reduce(fun s i -> s * i)


File.ReadAllLines(FName)
|> Array.map(fun s -> s |> Seq.map(fun c -> -48 + int c) |> Seq.toArray)
|> findRiskArea
