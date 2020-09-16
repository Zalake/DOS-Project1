open Akka.FSharp
open Akka.Actor
open System.Collections.Generic
let system = System.create "system" (Configuration.defaultConfig())
type SampleTypes= TupleType of int * int * int
let actorNum=10
//functions-------
let printerActor(mailbox: Actor<_>)=
    let rec loop()= actor{
        let! msg = mailbox.Receive()
        printfn "%i" msg
        return! loop()
    }
    loop()
let printer=spawn system "printer" printerActor
let printerActorList = List.init actorNum (fun i->spawn system ("printer"+string i) printerActor)
let lucasActor(mailbox: Actor<_>)=
    let mutable resList=new List<int>()
    let rec loop() = actor {
        let! TupleType(start,last,k) = mailbox.Receive()
        let mutable sqSum = 0
        let mutable sqRt = 0.0
        let mutable index=0
        for i = start to last do
            sqSum <- 0
            for j = i to (i+k-1) do
                sqSum <- sqSum + (j*j)
            sqRt <- sqrt (float sqSum)
            if sqRt = floor sqRt then
                printerActorList.Item(index)<!i
                index<-(index+1)%actorNum
        return! loop()
    }
    loop()

//defining handler function   
let handler (n:int, k:int) =
    //list of actors
    let actorList = List.init actorNum (fun i->spawn system (string i) lucasActor)
    let messagePerActor=200
    //let actorList = spawn system (string i) lucasActor
    let mutable index= 0
    let mutable counter=1;
    while counter<n do
        let start=counter
        let last=min n counter+messagePerActor-1
        actorList.Item(index)<!TupleType(start,last,k)
        counter<- (counter+messagePerActor)
        index<-(index+1)%actorNum

type Msg = InputType of int * int

//Defining boss actor
let boss(mailbox: Actor<_>)=
    let rec Bossloop() = actor {
         let! InputType(a,b) = mailbox.Receive()
         handler(a,b)
         return! Bossloop()
    }
    Bossloop()

let actorRef = spawn system "Boss" boss
actorRef <! InputType(1000000, 24)
System.Console.ReadLine() |> ignore
