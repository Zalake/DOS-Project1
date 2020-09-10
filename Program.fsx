// Learn more about F# at http://fsharp.org

open System

(*[<EntryPoint>]
let main (args : string[]) =
    if args.Length <> 2 then
        failwith "Error: Expected arguments <greeting> and <thing>"
    let last, k = args.[1], args.[2]*)
    

//for arg in fsi.CommandLineArgs do
 //   printfn "%s" arg

let mutable sqSum = 0
let mutable sqRt = 0.0
let start = 1 
let last = 40   //N - hardcoded the value as of now
let k = 24    // Hardcoded the value of K as of now

for i = start to last do
    sqSum <- 0
    for j = i to (i+k-1) do
        sqSum <- sqSum + (j*j)
    sqRt <- sqrt (float sqSum)
    if sqRt - floor sqRt = 0.0 then printfn "%i" i


   







       