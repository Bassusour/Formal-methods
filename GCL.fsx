#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer
#load "GraphPrinter.fsx"
open GraphPrinter
#load "printAST.fsx"
open PrintAST

let rec pow a b =
    match b with
    |0 -> 1
    |_ -> a * pow a (b-1);;

let rec eval e =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> eval(x) * eval (y)
    | DivExpr(x,y) -> eval(x) / eval (y)
    | PlusExpr(x,y) -> eval(x) + eval (y)
    | MinusExpr(x,y) -> eval(x) - eval (y)
    | PowExpr(x,y) -> pow (eval(x)) (eval(y))
    | UPlusExpr(x) -> eval(x) 
    | UMinusExpr(x) -> - eval(x);

let insertInt name (mapInts:Map<String, int>) = 
    match mapInts.TryFind name with
    | Some(n) -> mapInts
    | None    -> printfn "Enter initial value for %s" name
                 let input = int (Console.ReadLine())
                 mapInts.Add(name, input)
let insertArray name (mapArrays:Map<String, int[]>) = 
    match mapArrays.TryFind name with
    | Some(n) -> mapArrays
    | None    -> printfn "Enter initial value for %s: " name
                 mapArrays.Add (name, (Array.map int (Console.ReadLine().Replace(" ", "").Split [|','|])))


let rec setInitBool bool (mapInts, mapArrays) = 
    match bool with
    | TrueBool -> (mapInts, mapArrays)
    | FalseBool -> (mapInts, mapArrays)
    | OrBool(b1,b2) 
    | ScorBool(b1,b2)
    | AndBool(b1,b2)
    | ScandBool(b1,b2) -> setInitBool b1 (mapInts, mapArrays) |> setInitBool b2
    | NutBool(b1) -> setInitBool b1 (mapInts, mapArrays)
    | EqualBool(e1,e2) 
    | NeqBool(e1,e2) 
    | LtBool(e1,e2) 
    | GtBool(e1,e2) 
    | GeqBool(e1,e2) 
    | LeqBool(e1,e2) -> setInitVarExpr e1 (mapInts, mapArrays) |> setInitVarExpr e2
and setInitVarExpr expr (mapInts, mapArrays) = 
    match expr with
    | Num(x) -> (mapInts, mapArrays)
    | Var(x) -> (insertInt x mapInts, mapArrays)
    | Array(s,e) -> setInitVarExpr e (mapInts, (insertArray s mapArrays))
    | TimesExpr(x,y) 
    | DivExpr(x,y) 
    | PlusExpr(x,y) 
    | MinusExpr(x,y) 
    | PowExpr(x,y) -> let e1 = setInitVarExpr x (mapInts, mapArrays)
                      setInitVarExpr y e1
    | UPlusExpr(x)  
    | UMinusExpr(x) -> setInitVarExpr x (mapInts, mapArrays)
and setInitVars com (mapInts, mapArrays) = 
    match com with
    | AssCom(s,e) -> setInitVarExpr e ((insertInt s mapInts), mapArrays)
    | AssArrayCom(s,e1,e2) -> (mapInts, insertArray s mapArrays) |> (setInitVarExpr e1) |> (setInitVarExpr e2)
    | SkipCom -> (mapInts, mapArrays)
    | SemiCom(c1,c2) -> setInitVars c1 (mapInts, mapArrays) |> setInitVars c2 
    | IfCom(gc) 
    | DoCom(gc) -> setInitGC gc (mapInts, mapArrays)
and setInitGC gc (mapInts, mapArrays) = 
    match gc with
    | ArrowGc(b,c) -> setInitBool b (mapInts, mapArrays) |> setInitVars c
    | IfElseGc(gc1, gc2) -> setInitGC gc1 (mapInts, mapArrays) |> setInitGC gc2


let rec StepwiseCum q1 q2 (mapInts:Map<String, int>, mapArrays:Map<String, int []>) = function
    | AssCom(s,e) -> (mapInts.Add(s,eval(e)), mapArrays)
    | AssArrayCom(name,e1,e2) -> let arr = Map.find name mapArrays
                                 Array.set (arr) (eval(e1)) (eval(e2))
                                 (mapInts,mapArrays.Add(name, arr))

   
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "Enter (-SW | -ND | -D | -P) Guarded Command code : "
        //try
        let e = parse (Console.ReadLine())
        match e with 

            | (StepFlag, com) -> let (m1,m2) = setInitVars com (Map.empty, Map.empty)
                                 printfn "Map for ints %A \nMap for arrays %A" m1 m2
                                 StepwiseCum Qs Qf (m1,m2) com
                                 printfn "Map for ints %A \nMap for arrays %A" m1 m2
            | (PFlag, com) -> printCom com 0
                              printfn "Syntax is correct"
            | (det, com) -> makeNDGraph com det
        compute 1
        //with err -> printfn "Syntax Wrong"
        //            printfn "%s" err.Message
        //            compute 1

// Start interacting with the user
compute 1

