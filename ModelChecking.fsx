module ModelChecking
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
type NodeName = state
type Transition = Tc of Com | Tb of BoolExpr
type Neighbors = List<NodeName * Transition>
type AdjacencyList = Map<NodeName, Neighbors> 

//let mutable n = 0;;

let addToAdjacencyList (al:AdjacencyList) node1 node2 t = 
    match al.TryFind node1 with
    | Some (neighbors) -> al.Add(node1, (node2, t)::neighbors)
    | None -> al.Add(node1, [(node2,t)])

let rec makeProgramGraph c al n q1 q2 =
    match c with
    | AssCom(s,e) -> (addToAdjacencyList al q1 q2 (Tc c), n)
    | AssArrayCom(s,e1,e2) -> (addToAdjacencyList al q1 q2 (Tc c), n)
    | SkipCom -> (addToAdjacencyList al q1 q2 (Tc c), n)
    | SemiCom(c1, c2) -> let (newAL,newN) = makeProgramGraph c1 al (n+1) q1 (Q(n+1))
                         makeProgramGraph c2 newAL newN (Q(n+1)) q2 
    |IfCom(gc) -> let (alN,skrald) = gcPG gc al n q1 q2 None
                  alN
    |DoCom(gc) -> let ((newAl,newN), d) = gcPG gc al n q1 q1 None
                  (addToAdjacencyList newAl q1 q2 (Tb d), newN) 
and gcPG gc al n q1 q2 d = 
    match gc with
    | ArrowGc(b,c) -> let newAL = addToAdjacencyList al q1 (Q(n+1)) (Tb b)
                      match d with
                      |None -> ((makeProgramGraph c newAL (n+1) (Q(n+1)) q2), NutBool(b))
                      |Some x -> ((makeProgramGraph c newAL (n+1) (Q(n+1)) q2), AndBool(NutBool(b), x))
    | IfElseGc(ArrowGc(b,c), gc2) -> let ((newAL,newN), newD) = gcPG (ArrowGc(b,c)) al n q1 q2 d
                                     gcPG gc2 newAL newN q1 q2 (Some newD)



let reach1 state (al:AdjacencyList) = 
    match al.TryFind state with
    | Some(neighbors) -> neighbors
    | None -> []

let al = ['a', ['e'];
          'e', ['a'; 'h'];
          'h', ['g']; 
          'g', ['h']] 
          |> Map.ofList;;

//reach1 'e' al

let a = Set.ofList [1;2;3]
Set.toList a
a.Remove 1
a.Add 1
a.Contains 1

//getElementFromSet a

let getElementFromSet (s:Set<int>) = 
    match Set.toList s with
    | x::xs -> x

(*let algorithm (al:AdjacencyList) initialState = 
    let mutable visited = Set.empty
    let mutable toExplore = Set.singleton initialState
    while !toExplore.IsEmpty do
        let s = getElementFromSet toExplore
        let newExplore = toExplore.Remove s
        if( visited.Contains s) then 
            continue
        let newVisited = visited.Add s
        *)


let gclModelChecking com = 
    let programGraph = makeProgramGraph com Map.empty 0 Qs Qf
    printfn "%A" programGraph
    programGraph |> ignore