// Learn more about F# at http://fsharp.org

open System

open Akka.FSharp
open Akka.Actor

let system = System.create "system" (Configuration.defaultConfig())


type SampleTypes= TupleType of int * int
let mutable count=0
//functions-------
let squareSummation start k :bool =
    let mutable sqSum = 0
    let mutable sqRt = 0.0
    sqSum <- 0
    for i = start to (start+k-1) do
        sqSum <- sqSum + (i*i)
    sqRt <- sqrt (float sqSum)

    if sqRt - floor sqRt = 0.0 then
        true
    else
        false
(*let sss=squareSummation 9 24
if(sss)then
    printf "true"*)
//Actor code block
let lucasActor(mailbox: Actor<_>)=
    let rec loop() = actor {
        let! TupleType(x,y) = mailbox.Receive()
        if(squareSummation x y)then
            count <- count+1
        return! loop()
    }
    loop()
//intToString to create new actors
let int2String (x: int) = string x
let n=40 //hardcoded
let k=24 //hardcoded

for i=1 to n-k do
    let ActorName=int2String i
    //spawn new actor in every iteration and send the input tuple
    spawn system ActorName lucasActor <! TupleType(i,k)
System.Console.ReadLine() |> ignore
printfn "final answer is %i" count
System.Console.ReadLine() |> ignore


   







       
