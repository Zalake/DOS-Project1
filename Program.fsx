open Akka.FSharp
open Akka.Actor
open System.Collections.Generic
let system = System.create "system" (Configuration.defaultConfig())
type SampleTypes= TupleType of uint64 * uint64 * uint64
let actorNum=10
//functions-------
let printerActor(mailbox: Actor<_>)=
    let rec loop()= actor{
        let! msg = mailbox.Receive()
        let mutable res=string msg
        //res<-res.Substring(0,res.Length-2)
        printf "%s\n" res
        return! loop()
    }
    loop()
let printer=spawn system "printer" printerActor
let printerActorList = List.init actorNum (fun i->spawn system ("printer"+string i) printerActor)
let lucasActor(mailbox: Actor<_>)=
    let mutable resList=new List<int>()
    let rec loop() = actor {
        let! TupleType(start,last,k) = mailbox.Receive()
        let mutable sqSum:uint64 = 0UL
        let mutable sqRt:double = 0.0
        let mutable index=0
        for i = start to last do
            sqSum <- 0UL
            for j = i to (i+k-1UL) do
                sqSum <- sqSum + uint64 (j*j)
            sqRt <- sqrt (double sqSum)
            if sqRt = floor sqRt then
                printerActorList.Item(index)<!i
                index<-(index+1)%actorNum
        return! loop()
    }
    loop()

//defining handler function   
let handler (n:uint64, k:uint64) =
    //list of actors
    let actorList = List.init actorNum (fun i->spawn system (string i) lucasActor)
    let messagePerActor:uint64=200UL
    //let actorList = spawn system (string i) lucasActor
    let mutable index= 0
    let mutable counter:uint64=1UL;
    while counter<n do
        let start=uint64 counter
        let last=uint64 (min n (counter+messagePerActor-1UL))
        actorList.Item(index)<!TupleType(start,last,k)
        counter<- (counter+messagePerActor)
        index<-(index+1)%actorNum
    for i=0 to actorNum-1 do
        actorList.Item(i).Tell(PoisonPill.Instance)

type Msg = InputType of uint64 * uint64

//Defining boss actor
let boss(mailbox: Actor<_>)=
    let rec Bossloop() = actor {
         let! InputType(a,b) = mailbox.Receive()
         handler(a,b)
         return! Bossloop()
    }
    Bossloop()


let actorRef = spawn system "Boss" boss
actorRef <! InputType(100000000UL, 24UL)
actorRef.Tell(PoisonPill.Instance)

System.Console.ReadLine() |> ignore
