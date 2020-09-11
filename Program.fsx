open Akka.FSharp
open Akka.Actor

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
    if sqRt - floor sqRt = 0.0 then
        printf "%i\n" start
 
(*let sss=squareSummation 9 24
if(sss)then
    printf "true"*)
//Actor code block
let lucasActor(mailbox: Actor<_>)=
    let rec loop() = actor {
        let! TupleType(x,y) = mailbox.Receive()
        squareSummation x y
    }
    loop()
//intToString to create new actors
let int2String (x: int) = string x
let n=30
let k=2

for i=1 to n-k do
    let ActorName=int2String i
    //spawn new actor in every iteration and send the input tuple
    spawn system ActorName lucasActor <! TupleType(i,k)
System.Console.ReadLine() |> ignore
