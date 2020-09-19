#time "on"

//to run the code from command line
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 
open System
open Akka.FSharp
open Akka.Actor
open Akka.Configuration
open Akka.TestKit
open System.Collections.Generic

type dataTypes =
    |TupleType of uint64 * uint64 * uint64
    |Terminate of string
    |InputType of uint64 * uint64 * IActorRef
//to take inputs form command line
// printfn "total processor %i" Environment.ProcessorCount
let args : string array = fsi.CommandLineArgs |> Array.tail
let N = args.[0] |> uint64
let k = args.[1] |> uint64

let system = System.create "system" (Configuration.defaultConfig())
// type SampleTypes= TupleType of uint64 * uint64 * uint64
let actorNum=Environment.ProcessorCount
let mutable ActiveActors = 0
//functions-------
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
let KillerActor(mailbox: Actor<_>)=
    let rec loop()= actor{
        let! msg = mailbox.Receive()
        let mutable res=string msg
        //res<-res.Substring(0,res.Length-2)
        if(res="kill") then
            system.Terminate()|>ignore
        return! loop()
    }
    loop()
let printer=spawn system "printer" printerActor
let kill =spawn  system "killer" KillerActor
let mutable counter2=0
let calculator(start:uint64, last:uint64, k:uint64)=
    
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

        
let lucasActor(mailbox: Actor<_>)=
    let mutable resList=new List<int>()
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        
        match msg with
        |TupleType(start,last,k)-> calculator(start,last,k)
        
        return! loop()
    }
    loop()

//defining handler function   
let handler (n:uint64, k:uint64) =
    //list of actors
    let actorList = List.init actorNum (fun i->spawn system (string i) lucasActor)
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

//Defining boss actor
let boss(mailbox: Actor<_>)=
    let rec Bossloop() = actor {
         let! InputType(a,b,c) = mailbox.Receive()
         handler(a,b)
         return! Bossloop()
    }
    Bossloop()


let actorRef = spawn system "Boss" boss
actorRef <! InputType(N,k,actorRef)

let mutable x=0
while flag=true do
    x<-x+1
