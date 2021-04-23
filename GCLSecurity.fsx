open System

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

let rec allowedFlows adjacencyList initVars = 
    let list = Map.foldBack(fun x a -> x::a) initVars List.empty
and allowedFlows2 adjacencyList varsList resultingList = 
    match varsList with
    | x::xs -> allowedFlows3 adjacencyList varsList x
and allowedFlows3 adjacencyList varsList x resultingList = 
    match varsList with
    | y::ys -> if isFlow adjacencyList (snd x) (snd y) then (fst x, fst y)::resultingList
                                                            
and isFlow adjacencyList x y = 
    if x = y then true
    else match adjacencyList.TryFind x with
         | Some (parents) -> isFlowList parents y adjacencyList
         | None -> false
and isFlowList parents y adjacencyList = 
    match parents with
    | [] -> false
    | p::ps -> match isFlow adjacencyList p y with
               | true -> true
               | false -> isFlowList ps y adjacencyList


let gclSecurity =
    let makeLettuce = makeLettuce 
    let initialVariables = initVariables
    printfn "%A" makeLettuce
