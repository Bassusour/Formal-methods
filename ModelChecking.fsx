module ModelChecking
open System
open System.Net.NetworkInformation
#load "GCLTypesAST.fs"
//open GCLTypesAST
type NodeName = state
type Transition = Tc of Com | Tb of BoolExpr
type Neighbors = List<NodeName * Transition>
type AdjacencyList = Map<NodeName, Neighbors> 
type TransState = state * Map<string,int>
let rec evalMc e (mapInts:Map<String, int>) =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> evalMc x (mapInts) * evalMc y mapInts
    | DivExpr(x,y) -> evalMc x mapInts / evalMc y mapInts
    | PlusExpr(x,y) -> evalMc x mapInts + evalMc y mapInts
    | MinusExpr(x,y) -> evalMc x mapInts - evalMc y mapInts
    | PowExpr(x,y) -> pow (evalMc x mapInts) (evalMc y mapInts)
    | UPlusExpr(x) -> evalMc x mapInts 
    | UMinusExpr(x) -> - evalMc x mapInts;
    | Var(name) -> match mapInts.TryFind name with
                   | Some(x) -> x
                   | None    -> failwith "Variable not found"
    | Array(name, e) -> match mapInts.TryFind (name + (string (evalMc e mapInts))) with
                        | Some(a) -> a
                        | None    -> failwith "Array not found"

let rec evalMcBool b (mapInts:Map<String, int>) =
    match b with
    | TrueBool -> true 
    | FalseBool -> false 
    | EqualBool(e1,e2) -> evalMc e1 mapInts = evalMc e2 mapInts
    | GtBool(e1, e2)   -> evalMc e1 mapInts > evalMc e2 mapInts
    | GeqBool(e1, e2)  -> evalMc e1 mapInts >= evalMc e2 mapInts
    | LtBool(e1, e2)   -> evalMc e1 mapInts < evalMc e2 mapInts
    | LeqBool(e1, e2)  -> evalMc e1 mapInts <= evalMc e2 mapInts
    | ScandBool(b1,b2) | AndBool(b1, b2)  -> evalMcBool b1 mapInts && evalMcBool b2 mapInts
    | OrBool(b1, b2) | ScorBool(b1,b2) -> evalMcBool b1 mapInts || evalMcBool b2 mapInts
    | NutBool(b1) -> not(evalMcBool b1 mapInts)
let rec init() =
    printfn "Enter initial values"
    let input = string (Console.ReadLine())
    let inputList = List.ofArray (input.Replace(" ", "").Split [|','|])
    initMap inputList Map.empty
and initMap list map =
    match list with
    | [] -> map
    | s::xs -> let comp = s.Split[|'='|] 
               if(comp.[1].Contains '[') then initArray comp.[0] (formatA comp.[1]) xs 0 map
               else map.Add (comp.[0],int (comp.[1] )) |> initMap xs
and initArray name valueList xs c map =
    match valueList with
    |[] -> initMap xs map
    |y::ys -> let n = name + (string c) 
              map.Add (n,(int y)) |> initArray name ys xs (c+1)

and formatA (str:string) =
    Array.toList (str.Replace("[","").Replace("]","").Split [|';'|])


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



let rec reach1 state (al:AdjacencyList) = 
    match al.TryFind (fst state) with
    | Some(neighbors) -> getReachs (snd state) neighbors Set.empty
    | None -> Set.empty
and getReachs vars nbs reachable =
    match nbs with
    |[] -> reachable
    |x::xs -> match isReachable vars x with
              |Some (nState) -> getReachs vars xs (reachable.Add nState)
              |None -> getReachs vars xs reachable

and isReachable (vars:Map<String,int>) nb =
    match snd nb with
    |Tc c -> match c with
               | AssCom(s,e) -> let newVars = vars.Add (s, evalMc e vars)
                                Some(fst nb, newVars)
               | AssArrayCom(s,e1,e2) -> let newVars = vars.Add(s + (string (evalMc e1)), evalMc e2 vars)
                                         Some(fst nb, newVars)

               | SkipCom -> Some(fst nb, vars)
    |Tb b -> if (evalMcBool b vars) then Some(fst nb, vars) else None



let a = Set.ofList [1;2;3]
Set.toList a
a.Remove 1
a.Add 1
a.Contains 1

//getElementFromSet a

let getElementFromSet (s:Set<TransState>) = 
    match Set.toList s with
    | x::xs -> x

let algorithm (al:AdjacencyList) (initialState:TransState) = 
    let mutable visited = Set.empty
    let mutable toExplore = Set.singleton initialState
    let mutable stucks = Set.empty
    while not toExplore.IsEmpty do
        let s = getElementFromSet toExplore
        toExplore <- toExplore.Remove s
        if( visited.Contains s) then 
            ()
        else
           visited <- visited.Add s
           let reachables = reach1 s al
           if (reachables).IsEmpty then stucks <- stucks.Add s else toExplore <- Set.union toExplore reachables
    stucks
        
let rec prettyPrintFlemming (stucks:Set<TransState>) =
    printfn "Stuck States:"
    ppf2 (Set.toList stucks)
and ppf2 = function
    |[] -> ()
    | s::sx -> printf "%s: " (stateToString (fst s))
               printMap (Map.toList (snd s))
               printfn ""
               ppf2 sx

and printMap = function
    |[] -> ()
    |x::xs -> printf "%s = %d, " (fst x) (snd x)
              printMap xs

let gclModelChecking com = 
    let programGraph = makeProgramGraph com Map.empty 0 Qs Qf
    let initMap = init()
    let initState = (Qs, initMap)
    let stucks = algorithm (fst programGraph) initState
    prettyPrintFlemming stucks |> ignore