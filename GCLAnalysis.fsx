module GCLAnalysis
open System
#load "GCLTypesAST.fs"
//open GCLTypesAST
#load "AbstractCalculation.fsx"
open AbstractCalculation

type ArrSign = Map<string,Set<Sign>>
type VarSign  = Map<string,Sign>
type SignMem = VarSign * ArrSign
type Powerset = Set<SignMem>
type Analysis = Map<state,Powerset>


let find (map:Map<'k,'v>) key =
    match map.TryFind key with
    |Some(e) -> e
    |None -> failwith "could not find waldo"
let numS = function
    |x when x > 0 -> Plus
    |x when x < 0 -> Minus
    |_ -> Zero

let stringToSign str =
    match str with
    |"+" -> Plus
    |"-" -> Minus
    |"0" -> Zero
    | _  -> failwith "not a sign"

let signToString sign =
    match sign with
    |Plus -> "+"
    |Minus -> "-"
    |Zero -> "0"

let signsToString signs =
   let str = "{" + (Set.foldBack (fun s a -> (signToString s) + "," + a) signs "")
   str.[0..(str.Length-2)] + "}"

let rec stringToSigns strings (set:Set<Sign>) =
    match strings with
    |[] -> set
    |x::xs -> stringToSigns xs (set.Add(stringToSign x ))
    
let innerMap set f sign = let newSet = Set.map (f sign) set
                          Set.foldBack (Set.union) newSet Set.empty

let calcSet (set1) (set2) (f) = 
    let newSet =Set.map (innerMap set2 f) set1
    Set.foldBack (Set.union) newSet Set.empty

let calcSingleSet set f =
    Set.foldBack 

let rec exprSign e (mem:SignMem) =
  match e with
    | Num(x) -> Set.singleton (numS x)
    | TimesExpr(x,y) -> calcSet (exprSign x (mem)) (exprSign y (mem)) timesS
    | DivExpr(x,y) -> calcSet (exprSign x (mem)) (exprSign y (mem)) divideS
    | PlusExpr(x,y) -> calcSet (exprSign x (mem)) (exprSign y (mem)) addS
    | MinusExpr(x,y) -> calcSet (exprSign x (mem)) (exprSign y (mem)) subS
    | PowExpr(x,y) -> calcSet (exprSign x (mem)) (exprSign y (mem)) powS
    | UPlusExpr(x) -> exprSign x (mem) 
    | UMinusExpr(x) ->  uminusSetS (exprSign x (mem));
    | Var(name) -> match (fst mem).TryFind name with
                   | Some(x) -> Set.singleton x
                   | None    -> failwith "Variable not found"
    | Array(name, e) -> match (snd mem).TryFind name with
                        | Some(a) -> if not ((Set.intersect (exprSign e mem) (Set.ofList [Zero; Plus])).IsEmpty) then a
                                     else Set.empty
                        | None    -> failwith "Array not found";


let rec boolSign b (mem:SignMem) = 
    match b with
    | TrueBool -> Set.singleton TrueBool
    | EqualBool(e1,e2) -> calcSet(exprSign e1 (mem)) (exprSign e2 (mem)) equalS
    | GtBool(e1,e2) -> calcSet(exprSign e1 (mem)) (exprSign e2 (mem)) greaterS
    | GeqBool(e1,e2) -> calcSet(exprSign e1 (mem)) (exprSign e2 (mem)) greaterEqualS
    | AndBool(b1,b2) 
    | ScandBool(b1,b2) -> calcSet(boolSign b1 (mem)) (boolSign b2 (mem)) andS
    | NutBool(b1) -> Set.map nutS (boolSign b1 mem) 
    | OrBool(b1,b2)
    | ScorBool(b1,b2) -> calcSet(boolSign b1 (mem)) (boolSign b2 (mem)) orS
    | NeqBool(e1,e2) -> calcSet(exprSign e1 (mem)) (exprSign e2 (mem)) notEqualS
    | LtBool(e1,e2) -> calcSet(exprSign e1 (mem)) (exprSign e2 (mem)) lessS
    | LeqBool(e1,e2) -> calcSet(exprSign e1 (mem)) (exprSign e2 (mem)) lessEqualsS

let rec assignAllMems n e (ps:Powerset) = 
   Set.foldBack Set.union (Set.map (assignAllMemsH n e) ps) Set.empty
and assignAllMemsH n e signmem = 
    match signmem with
    | (var,arr) -> let signs = (exprSign e (var,arr))
                   Set.map (setVar var arr n) signs
and setVar varSign varArray n sign = 
    (varSign.Add (n,sign), varArray)  

let addToAnal q ps (a:Analysis) =
    match a.TryFind q with
    |Some(ps2) -> a.Add(q, Set.union ps ps2)
    |None      -> a.Add(q, ps)

let rec assignArray n e (ps:Powerset) = 
    Set.foldBack Set.union (Set.map (assArrInMem n e) ps) Set.empty
and assArrInMem n e sm =
    match sm with
    |(var,arr) -> let eSigns = (exprSign e sm)
                  let aSigns = find arr n
                  let mutable rest = Set.empty
                  for s in eSigns do
                     let noDel = (var,arr.Add (n, (Set.add s aSigns)))
                     let del = Set.map (setAVar var arr n s aSigns) aSigns
                     rest <- Set.add noDel (Set.union del rest) 
                  rest
and setAVar var arr n eSigns aSigns signTR =
    (var,arr.Add (n, (Set.union (aSigns.Remove signTR) (Set.singleton eSigns))))

let isPosetiv e1 mem =
    not ((Set.intersect (exprSign e1 mem) (Set.ofList [Zero; Plus])).IsEmpty)

let rec getDet gc =
    match gc with
    |ArrowGc(b,c) -> NutBool(b) 
    |IfElseGc(gc1, gc2) -> let b = getDet gc1
                           AndBool(b, getDet gc2)

let rec evalComm q1 q2 ((a:Analysis),n) comm = 
    match comm with
    | AssCom(name,e) -> (addToAnal q2 (assignAllMems name e (find a q1)) a,n)
    | AssArrayCom(name,e1,e2) -> if not(Set.exists (isPosetiv e1) (find a q1)) then (a,n)
                                 else (addToAnal q2 (assignArray name e2 (find a q1)) a,n)
    |SkipCom -> (a,n)
    |SemiCom(c1,c2) -> let anal = evalComm q1 (Q(n+1)) (a,(n+1)) c1
                       evalComm (Q(n+1)) q2 anal c2
    |IfCom(gc) -> gcSign q1 q2 (a,n) gc
    |DoCom(gc) -> let newAnal = gcSign q1 q1 (a,n) gc
                  match newAnal with
                  |(newA, n2) when newA <> a -> evalComm q1 q2 (newA,n) (DoCom(gc))
                  |(newA, n2) -> let ps = find a q1
                                 let f mem = Set.contains TrueBool (boolSign (getDet gc) mem)
                                 let anal = addToAnal (q2) (Set.filter f ps) newA
                                 (anal,n2)
                        
and gcSign q1 q2 (a,n) gc =
    match gc with
    |ArrowGc(b,c) -> let ps = find a q1
                     let f mem = Set.contains TrueBool (boolSign b mem)
                     let anal = addToAnal (Q(n+1)) (Set.filter f ps) a
                     evalComm (Q(n+1)) q2 (anal,n+1) c
    |IfElseGc(gc1, gc2) -> let anal = gcSign q1 q2 (a,n) gc1
                           gcSign q1 q2 anal gc2



   // let maps = evalCum q1 (Q(n+1)) (mapInts, mapArrays, (n+1)) c1
   //evalCum (Q(n+1)) q2 maps c2

let insertSignInt name (sa:SignMem) = 
    match (fst sa).TryFind name with
    | Some(n) -> sa
    | None    -> printfn "Enter initial sign for %s" name
                 let input = stringToSign (Console.ReadLine())
                 ((fst sa).Add (name, input), snd sa)
let insertSignArray name (sa:SignMem) = 
    match (snd sa).TryFind name with
    | Some(n) -> sa
    | None    -> printfn "Enter initial signs for %s: " name
                 (fst sa, (snd sa).Add(name, stringToSigns (List.ofArray(Console.ReadLine().Replace(" ", "").Split [|','|])) Set.empty))
                 


let rec setInitSignBool bool sa = 
    match bool with
    | TrueBool -> sa
    | FalseBool -> sa
    | OrBool(b1,b2) 
    | ScorBool(b1,b2)
    | AndBool(b1,b2)
    | ScandBool(b1,b2) -> setInitSignBool b1 sa |> setInitSignBool b2
    | NutBool(b1) -> setInitSignBool b1 sa
    | EqualBool(e1,e2) 
    | NeqBool(e1,e2) 
    | LtBool(e1,e2) 
    | GtBool(e1,e2) 
    | GeqBool(e1,e2) 
    | LeqBool(e1,e2) -> setInitSignExpr e1 sa |> setInitSignExpr e2
and setInitSignExpr expr sa = 
    match expr with
    | Num(x) -> sa
    | Var(x) -> insertSignInt x sa
    | Array(s,e) -> setInitSignExpr e (insertSignArray s sa)
    | TimesExpr(x,y) 
    | DivExpr(x,y) 
    | PlusExpr(x,y) 
    | MinusExpr(x,y) 
    | PowExpr(x,y) ->    setInitSignExpr x sa |> setInitSignExpr y
    | UPlusExpr(x)  
    | UMinusExpr(x) -> setInitSignExpr x sa
and setInitSign com (sa:SignMem) = 
    match com with
    | AssCom(s,e) -> setInitSignExpr e (insertSignInt s sa)
    | AssArrayCom(s,e1,e2) -> insertSignArray s sa |> (setInitSignExpr e1) |> (setInitSignExpr e2)
    | SkipCom -> sa 
    | SemiCom(c1,c2) -> setInitSign c1 sa |> setInitSign c2 
    | IfCom(gc) 
    | DoCom(gc) -> setInitSignGC gc sa
and setInitSignGC gc sa = 
    match gc with
    | ArrowGc(b,c) -> setInitSignBool b sa |> setInitSign c
    | IfElseGc(gc1, gc2) -> setInitSignGC gc1 sa |> setInitSignGC gc2;


                                                


let rec beginInit c n (ps:Powerset) com =   
    match c with
    |x when x = n -> ps
    |_ ->  printfn "Enter signs for memory %d" c
           beginInit (c+1) n (ps.Add (setInitSign com (Map.empty,Map.empty))) com

let rec printAnalysis c n (a:Analysis) =
    match c with
    |x when x > n -> c
    |x -> printPowerSet (Q(x)) (find a (Q(x)))
          printAnalysis (c+1) n a
and printPowerSet q ps =
    printfn "%s:" (stateToString q)
    for mem in ps do
        printSignMem mem
        printfn ""
and printSignMem (var, arr) =
    for v in var do
        printf "%s: %s  "  v.Key (signToString v.Value)
    for a in arr do
        printf "%s: %s  "  a.Key (signsToString a.Value)
    


let signAnalysis com =
    printfn "How many abstract memories do you want?"
    let n = int (Console.ReadLine())
    let sa = beginInit 0 n Set.empty com
    let mutable a = Map.empty
    a <- a.Add (Qs, sa)
    let (anal, nStates) = evalComm Qs Qf (a,0) com
    printPowerSet Qs (find anal Qs)
    printAnalysis 1 nStates anal
    printPowerSet Qf (find anal Qf)

