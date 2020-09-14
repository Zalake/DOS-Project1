open Akka.FSharp
open Akka.Actor
open System.Collections.Generic
let system = System.create "system" (Configuration.defaultConfig())


type SampleTypes= TupleType of int * int
//functions-------
let squareSummation start k =
    let mutable sqSum = 0
    let mutable sqRt = 0.0
    sqSum <- 0
    for i = start to (start+k-1) do
        sqSum <- sqSum + (i*i)
    sqRt <- sqrt (float sqSum)
    if sqRt = floor sqRt then
        printf "%i\n" start

let lucasActor(mailbox: Actor<_>)=
    let rec loop() = actor {
        let! TupleType(x,y) = mailbox.Receive()
        squareSummation x y
        return! loop()
    }
    loop()
//intToString to create new actors
let n=100000000
let k=24
//number of actors
let actorNum=10
//list of actors
let actorList = List.init actorNum (fun i->spawn system (string i) lucasActor)
let mutable index= 0
let mutable counter=1;
while counter<n do
    if counter % actorNum=0 then
        index<-(index+1)%actorNum
    actorList.Item(index)<!TupleType(counter,k)
    counter<- counter+1
    
System.Console.ReadLine() |> ignore
