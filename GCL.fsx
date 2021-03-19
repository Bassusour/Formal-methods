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

let rec setInitVars com mapVars mapArrays = 
    match com with
    | AssCom(s,e) -> setInitVars e (insertInt s mapVars) mapArrays
and insertInt name (mapVars:Map<String, int>) = 
    match mapVars.TryFind name with
    | Some(n) -> mapVars
    | None    -> printf "Enter initial value for %s" name
                 let input = int (Console.ReadLine())
                 mapVars.Add(name, input)
and insertArray name (mapArrays:Map<String, int[]>) = 
    match mapArrays.TryFind name with
    | Some(n) -> mapArrays
    | None    -> printf "Enter initial value for %s" name
                 mapArrays.Add (name, (Array.map int (Console.ReadLine().Replace(" ", "").Split [|','|])))

let rec insertArray name (mapArrays:Map<string,int[]>) = 
    match map

let rec setInitVarExpr expr map = 
    match expr with
    | Num(x) -> map
    | Var(x) -> insert x map
    | Array(s,e) -> 
    | TimesExpr(x,y) -> printfn "*"
                        printExpr(x) (l+1) 
                        printExpr(y) (l+1) 
    | DivExpr(x,y) ->  printfn "/"
                       printExpr(x) (l+1) 
                       printExpr(y) (l+1) 
    | PlusExpr(x,y) -> printfn "+"
                       printExpr(x) (l+1) 
                       printExpr(y) (l+1) 
    | MinusExpr(x,y) -> printfn "-"
                        printExpr (x) (l+1) 
                        printExpr (y) (l+1) 
    | PowExpr(x,y) -> printfn "^"
                      printExpr(x) (l+1) 
                      printExpr(y) (l+1) 
    | UPlusExpr(x) -> printfn "+"
                      printExpr(x) (l+1) 
    | UMinusExpr(x) -> printfn "."
                       printExpr(x) (l+1)

let rec StepwiseCum q1 q2 = function
    |AssCom(s,e) -> 




   

   
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
        printfn "Enter (-ND | -D | -P) Guarded Command code : "
        try
        let e = parse (Console.ReadLine())
        match e with 
            | (StepFlag, com) -> StepwiseCum Qs Qf com
            | (PFlag, com) -> printCom com 0
                              printfn "Syntax is correct"
            | (det, com) -> makeNDGraph com det
        compute 1
        with err -> printfn "Syntax Wrong"
                    printfn "%s" err.Message
                    compute 1

// Start interacting with the user
compute 1
