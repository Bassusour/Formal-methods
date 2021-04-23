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
#load "GCLAnalysis.fsx"
open GCLAnalysis

let rec pow a b =
    match b with
    |0 -> 1
    |_ -> a * pow a (b-1);;

let rec eval e (mapInts:Map<String, int>, mapArrays:Map<String, int []>) =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> eval x (mapInts, mapArrays) * eval y (mapInts, mapArrays)
    | DivExpr(x,y) -> eval x (mapInts, mapArrays) / eval y (mapInts, mapArrays)
    | PlusExpr(x,y) -> eval x (mapInts, mapArrays) + eval y (mapInts, mapArrays)
    | MinusExpr(x,y) -> eval x (mapInts, mapArrays) - eval y (mapInts, mapArrays)
    | PowExpr(x,y) -> pow (eval x (mapInts, mapArrays)) (eval y (mapInts, mapArrays))
    | UPlusExpr(x) -> eval x (mapInts, mapArrays) 
    | UMinusExpr(x) -> - eval x (mapInts, mapArrays);
    | Var(name) -> match mapInts.TryFind name with
                   | Some(x) -> x
                   | None    -> failwith "Variable not found"
    | Array(name, e) -> match mapArrays.TryFind name with
                        | Some(a) -> a.[eval e (mapInts, mapArrays)]
                        | None    -> failwith "Array not found"

let rec evalBool b (mapInts:Map<String, int>, mapArrays:Map<String, int []>) = 
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

let rec evalCum q1 q2 (mapInts:Map<String, int>, mapArrays:Map<String, int []>, n) c =
    try
    match c with
    | AssCom(s,e) -> (mapInts.Add(s,eval e (mapInts, mapArrays)), mapArrays, n)
    | AssArrayCom(name,e1,e2) -> let arr = Map.find name mapArrays
                                 Array.set (arr) (eval e1 (mapInts, mapArrays)) (eval e2 (mapInts, mapArrays))
                                 (mapInts,mapArrays.Add(name, arr), n)
    | SkipCom -> (mapInts, mapArrays, n)
    | SemiCom(c1, c2) -> let maps = evalCum q1 (Q(n+1)) (mapInts, mapArrays, (n+1)) c1
                         evalCum (Q(n+1)) q2 maps c2
    | IfCom(gc) -> match evalGC gc (mapInts, mapArrays) with 
                   |Some(c) -> evalCum (Q(n+1)) q2 (mapInts, mapArrays, (n+1)) c
                   |None -> raise (StuckException(q1, mapInts, mapArrays))
    |DoCom(gc) -> match evalGC gc (mapInts, mapArrays) with 
                   |Some(c) -> let maps = evalCum (Q(n+1)) q2 (mapInts, mapArrays, (n+1)) c
                               evalCum q1 q2 maps (DoCom(gc))
                   |None -> (mapInts, mapArrays,n)
    with 
      |StuckException(err) -> raise (StuckException(err))
      | _ -> raise (StuckException(q1, mapInts, mapArrays))
and evalGC gc (mapInts, mapArrays) = 
    match gc with
    | ArrowGc(b,c) when evalBool b (mapInts, mapArrays) ->  Some(c)
    | ArrowGc(b,c) -> None 
    | IfElseGc(gc1, gc2) -> match evalGC gc1 (mapInts,mapArrays) with
                            |Some(c) -> Some(c)
                            |None -> evalGC gc2 (mapInts,mapArrays)

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
        printfn "Enter ( -SEC | -SA | -SW | -ND | -D | -P) Guarded Command code : "
        try
        let e = parse (Console.ReadLine())
        match e with 
            | (StepFlag, com) -> let (m1,m2) = setInitVars com (Map.empty, Map.empty)
                                 //printfn "Map for ints %A \nMap for arrays %A" m1 m2
                                 try
                                   let (mNew1, mNew2, n) = evalCum Qs Qf (m1,m2,0) com 
                                   printfn "Status:Terminated\nNode: %s" (stateToString Qf)
                                   Map.map (fun s i -> printfn "%s: %d" s i) mNew1 |> ignore
                                   Map.map (fun s a -> printfn "%s: %A" s a) mNew2 |> ignore
                                 with
                                 |StuckException(q, sm1, sm2) -> printfn "Status:Stuck\nNode: %s" (stateToString q)
                                                                 Map.map (fun s i -> printfn "%s: %d" s i) sm1 |> ignore
                                                                 Map.map (fun s a -> printfn "%s: %A" s a) sm2 |> ignore
                                 //printfn "Map for ints %A \nMap for arrays %A" mNew1 mNew2

            | (PFlag, com) -> printCom com 0
                              printfn "Syntax is correct"
            | (SAFlag, com) -> signAnalysis com
            | (det, com) -> makeNDGraph com det
            | (SECFlag, com) -> gclSecurity com
        compute 1
        with err -> printfn "Syntax Wrong"
                    printfn "%s" err.Message
                    compute 1

// Start interacting with the user
compute 1
