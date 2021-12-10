open System
open System.IO

let FName = "Day09/sample.txt"
// let FName = "Day09/input.txt"




// part 1 (506)
let findRiskLevel (map : int[][])=

    let mx = map.[0].Length
    let my = map.Length

    let getNeighbourIndices (x, y) =
        [| x-1, y; x, y-1; x+1, y; x, y+1|]
        |> Array.filter(fun (x,y) -> x > -1 && x < mx && y > -1 && y < my)


    [| 0 .. my-1 |]
    |> Array.collect(fun y -> [| 0 .. mx-1 |] |> Array.map(fun x-> x,y))
    |> Array.map(fun (x,y) -> map.[y].[x], getNeighbourIndices (x,y) |> Array.map(fun (x,y) -> map.[y].[x]))
    |> Array.map(fun (i,a) -> i, i < (a |> Array.min))
    |> Array.filter snd
    |> Array.sumBy(fun (i, _) -> i+1)


File.ReadAllLines(FName)
|> Array.map(fun s -> s |> Seq.map(fun c -> -48 + int c) |> Seq.toArray)
|> findRiskLevel



// part 2
let map =
    File.ReadAllLines(FName)
    |> Array.map(fun s -> s |> Seq.map(fun c -> -48 + int c) |> Seq.toArray)


let mx = map.[0].Length
let my = map.Length

let getNeighbourIndices (x, y) =
    [| x-1, y; x, y-1; x+1, y; x, y+1|]
    |> Array.filter(fun (x,y) -> x > -1 && x < mx && y > -1 && y < my)
    |> Set.ofArray

let lowPoints =
    [| 0 .. my-1 |]
    |> Array.collect(fun y -> [| 0 .. mx-1 |] |> Array.map(fun x-> x,y))
    |> Array.map(fun (x,y) -> (x,y),map.[y].[x], getNeighbourIndices (x,y) |> Set.map(fun (x,y) -> map.[y].[x]))
    |> Array.filter(fun (I,i,a) -> i < (a |> Set.toArray |> Array.min))
    |> Array.map(fun (a,_,_) -> a)

let x,y = lowPoints.[1]
// val it : (int * int) [] = [|(1, 0); (9, 0); (2, 2); (6, 4)|]


let getBasinSize x y =

    let rec f (x,y) s =
        let n = 
            getNeighbourIndices (x,y)
            |> Set.filter(fun (x,y) -> map.[y].[x] < 9)
            |> Set.filter(fun i -> s |> Set.contains i |> not)

        match n with
        | s' when s'.IsEmpty -> s
        | s' -> 
            s'
            |> Set.fold (fun s'' i -> f i s'') s'


    let s = (set [(x,y)])
    f (x,y) s


let s = set [(7, 0); (8, 0); (8, 1); (9, 0); (9, 1); (9, 2)]
s |> Set.contains (7,0)

getNeighbourIndices (x, y)

getBasinSize x y

[|0,0|] |> Array.collect(fun (x,y) -> getBasinSize x y)