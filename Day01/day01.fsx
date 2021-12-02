
//let fname = @"Day01/sample.txt"
let fname = @"Day01/input.txt"

System.IO.File.ReadAllLines(fname)
|> Array.map(int)
|> Array.pairwise
|> Array.filter(fun (a,b) -> a < b)
|> Array.length



let window3 (array: 'T[]) =
    if array.Length < 3 then Array.empty else
    Array.init (array.Length-2) (fun i -> array.[i], array.[i+1], array.[i+2])



System.IO.File.ReadAllLines(fname)
|> Array.map(int)
|> window3
|> Array.map (fun (a,b,c) -> a+b+c)
|> Array.pairwise
|> Array.filter(fun (a,b) -> a < b)
|> Array.length
