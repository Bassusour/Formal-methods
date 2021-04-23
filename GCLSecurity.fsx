open System
#load "GCLTypesAST.fs"
open GCLTypesAST
type SecName = string
type Parents = List<SecName>
type AdjacencyList = Map<string, Parents> 
type flow = string * string


let rec makeLettuce = 
    printfn "Enter security lattice"
    let input = string (Console.ReadLine())
    let inputList = List.ofArray (input.Replace(" ", "").Split [|','|])
    initLattice inputList Map.empty
and initLattice list am = 
    match list with
    | [] -> am
    | s::xs -> let component = s.Split[|'<'|] 
               let amAdded = addToAdjacencyMatrix am component.[1] component.[0]
               initLattice xs amAdded
and addToAdjacencyMatrix am parent child = 
    match am.TryFind child with
    | Some (parents) -> am.Add(child, parent::parents)
    | None -> am.Add(child, [parent])

let rec initVariables = 
    printfn "Enter initial variable security"
    let input = string (Console.ReadLine())
    let inputList = List.ofArray (input.Replace(" ", "").Split [|','|])
    setVarSecurity Map.empty inputList
and setVarSecurity map list = 
    match list with
    | [] -> map
    | x::xs -> let components = x.Split[|'='|]
               let mapAdded = map.Add(components.[0], components.[1])
               setVarSecurity mapAdded xs

let rec allowedFlows (adjacencyList:AdjacencyList) initVars = 
    let list = Map.fold(fun a k v  -> (k,v)::a)  List.empty initVars
    allowedFlows2 adjacencyList list list []
and allowedFlows2 adjacencyList queue varsList resultingList = 
    match queue with
    | x::xs -> let rlNew =allowedFlows3 adjacencyList varsList x resultingList
               allowedFlows2 adjacencyList xs varsList rlNew
    |[] -> resultingList
and allowedFlows3 adjacencyList varsList x resultingList = 
    match varsList with
    | y::ys -> match isFlow adjacencyList (snd x) (snd y) with 
                |true -> allowedFlows3 adjacencyList ys x ((fst x, fst y)::resultingList)
                |false -> allowedFlows3 adjacencyList ys x resultingList
    |[] -> resultingList
                                                          
and isFlow adjacencyList x y = 
    if x = y then true else
    match adjacencyList.TryFind x with
         | Some (parents) -> isFlowList parents y adjacencyList
         | None -> false
and isFlowList parents y adjacencyList = 
    match parents with
    | [] -> false
    | p::ps -> match isFlow adjacencyList p y with
               | true -> true
               | false -> isFlowList ps y adjacencyList


let rec secExpr e =
  match e with
    | Num(x) -> []
    | TimesExpr(x,y) -> (secExpr x)@(secExpr y) 
    | DivExpr(x,y) -> (secExpr x)@(secExpr y) 
    | PlusExpr(x,y) -> (secExpr x)@(secExpr y) 
    | MinusExpr(x,y) -> (secExpr x)@(secExpr y) 
    | PowExpr(x,y) -> (secExpr x)@(secExpr y) 
    | UPlusExpr(x) -> (secExpr x) 
    | UMinusExpr(x) -> (secExpr x) 
    | Var(name) -> [name]
    | Array(name, e) -> [name]@(secExpr e)

let rec secBool b (mapInts:Map<String, int>, mapArrays:Map<String, int []>) = 
    match b with
    | TrueBool -> true 
    | FalseBool -> false 
    | EqualBool(e1,e2) -> eval e1 (mapInts, mapArrays) = eval e2 (mapInts, mapArrays)
    | GtBool(e1, e2)   -> eval e1 (mapInts, mapArrays) > eval e2 (mapInts, mapArrays)
    | GeqBool(e1, e2)  -> eval e1 (mapInts, mapArrays) >= eval e2 (mapInts, mapArrays)
    | LtBool(e1, e2)   -> eval e1 (mapInts, mapArrays) < eval e2 (mapInts, mapArrays)
    | LeqBool(e1, e2)  -> eval e1 (mapInts, mapArrays) <= eval e2 (mapInts, mapArrays)
    | ScandBool(b1,b2) | AndBool(b1, b2)  -> evalBool b1 (mapInts, mapArrays) && evalBool b2 (mapInts, mapArrays)
    | OrBool(b1, b2) | ScorBool(b1,b2) -> evalBool b1 (mapInts, mapArrays) || evalBool b2 (mapInts, mapArrays)
    | NutBool(b1) -> not(evalBool b1 (mapInts, mapArrays))

let mixMatch inFlows outFlows =
    let mutable list = [] 
    for f1 in inFlows do
        for f2 in outFlows do
            list <- (f1,f2)::list

let rec secCum af =
    match c with
    | AssCom(s,e) -> let inFlow = secExpr e
                     mixMatch inFlow [s]
    | AssArrayCom(name,e1,e2) -> let inFlow = (secExpr e1) @ (secExpr e2)
                                 mixMatch inFlow [name]
    | SkipCom -> 
    | SemiCom(c1, c2) -> 
    |IfCom(gc) ->  
    |DoCom(gc) -> 
and secGC gc (mapInts, mapArrays) = 
    match gc with
    | ArrowGc(b,c) -> 
    | ArrowGc(b,c) ->
    | IfElseGc(gc1, gc2) -> 


let gclSecurity com =
    let lettuce = makeLettuce 
    let initialVariables = initVariables
    let allowedFlows = allowedFlows lettuce initialVariables
    let actualFlows = secCom com []
    printfn "%A" allowedFlows

gclSecurity