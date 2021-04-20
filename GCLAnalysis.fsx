module GCLAnalysis
open System
open System.Numerics
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "AbstractCalculation.fsx"
open AbstractCalculation

type Sign =
    |Zero
    |Plus
    |Minus

type ArrSign = Map<string,Set<Sign>>
type VarSign  = Map<string,Sign>
type SignMem = VarSign * ArrSign
type Powerset = List<SignMem>
type Analysis = Map<state,Powerset>

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

let rec stringToSigns strings (set:Set<Sign>) =
    match strings with
    |[] -> set
    |x::xs -> stringToSigns xs (set.Add(stringToSign x ))
let innerMap set f sign = let newSet = Set.map (f sign) set
                          Set.foldBack (Set.union) newSet Set.empty
let calcSet (set1:Set<Sign>) (set2:Set<Sign>) (f:Sign->Sign->Set<Sign>) = 
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
                        | Some(a) -> if not ((Set.union (exprSign e mem) (Set.ofList [Zero; Plus])).IsEmpty) then a
                                     else Set.empty
                        | None    -> failwith "Array not found";






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
    printfn "Enter signs for memorie %d" c
    match c with
    |x when x = n -> ps
    |_ -> beginInit (c+1) n ((setInitSign com (Map.empty,Map.empty))::ps) com


let signAnalysis com =
    printfn "How many abstract memories do you want?"
    let n = int (Console.ReadLine())
    let sa = beginInit 0 n List.empty com
    printfn "%A" sa