#time "on"

//nuget packages
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka.FSharp
open Akka.Actor
open Akka.Configuration
open Akka.TestKit
open System.Collections.Generic

type DataTypes =
    |TupleType of uint64 * uint64 * uint64
    |Terminate of string
    |InputType of uint64 * uint64

//Taking inputs form command line Interface
//printfn "number of processor in this syetm are total processor %i" Environment.ProcessorCount
let args : string array = fsi.CommandLineArgs |> Array.tail
let N = args.[0] |> uint64
let k = args.[1] |> uint64

let system = System.create "system" (Configuration.defaultConfig())

let actorNum=10 //defining actor numbers
let mutable ActiveActors = 0  //counr of active actors

// Printer actor to print the results
//after computation by the worker actor
let mutable flag =true
let printerActor(mailbox: Actor<_>)=
    let rec loop()= actor{
        let! msg = mailbox.Receive()
        let mutable res=string msg
        if(res = "-1") then
            flag <- false
        else
            printf "%s\n" res
        return! loop()
    }
    loop()
    
let printer=spawn system "printer" printerActor
let mutable counter2=0

// Implementation of worker actors takes inputs start, last and K 
// It invokes the function calculator       
let lucasActor(mailbox: Actor<_>)=
    let mutable resList=new List<int>()
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        match msg with
        | TupleType(start,last,k)->
            let mutable sqSum:uint64 = 0UL
            let mutable sqRt:double = 0.0
            for i = int start to int last do
                sqSum <- 0UL
                for j = i to (i+(int k)-(int 1UL)) do
                    sqSum <- sqSum +  (uint64 j*uint64 j)
                sqRt <- sqrt (double sqSum)
                if sqRt = floor sqRt then
                    printer<!i
            counter2<-counter2+1
            if(counter2=int ActiveActors)then
                printer<! (-1)
        | _ -> //do nothing
        return! loop()
    }
    loop()

// A function which is called by Boss. It takes n and k as inputs
// and defines the logic to spawn worker actors.*)
let handler (n:uint64, k:uint64) =
    let actorList = List.init actorNum (fun i->spawn system (string i) lucasActor) //list of actors
    let mutable messagePerActor:uint64=N/(uint64 actorNum)
    if( int messagePerActor <1) then
        ActiveActors<- int N
        messagePerActor<-1UL
    else
        ActiveActors <- actorNum
    let mutable index= 0
    let mutable counter:uint64=1UL;
    while counter<=n do
        let start=uint64 counter
        let last=uint64 (min n (counter+messagePerActor-1UL))
        actorList.Item(index)<!TupleType(start,last,k)
        counter<- (counter+messagePerActor)
        index<-(index+1)%actorNum

// Boss actor implementation. It calls the function
// handler to distrubute the task among the actors
let boss(mailbox: Actor<_>)=
    let rec Bossloop() = actor {
         let! msgs = mailbox.Receive()
         match msgs with
         |InputType(a,b) -> handler(a,b)
         |_ -> //do nothing
         return! Bossloop()
    }
    Bossloop()

// Spawning the boss actor
// passing inputs N and K
let actorRef = spawn system "Boss" boss
actorRef <! InputType(N,k)

let mutable x=0
while flag = true do
    x<-x
