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


let rec pow a b =
    match b with
    |0 -> 1
    |_ -> a * pow a (b-1);;



//let rec eval e =
//  match e with
//    | Num(x) -> x
//    | TimesExpr(x,y) -> eval(x) * eval (y)
//   | DivExpr(x,y) -> eval(x) / eval (y)
//    | PlusExpr(x,y) -> eval(x) + eval (y)
//    | MinusExpr(x,y) -> eval(x) - eval (y)
//    | PowExpr(x,y) -> pow (eval(x)) (eval(y))
//    | UPlusExpr(x) -> eval(x) 
//    | UMinusExpr(x) -> - eval(x);
let rec printSpace = function
    |0 -> printf "|"
    |x -> printf "  " 
          printSpace (x-1);; 

    


let rec printExpr e l=
    printSpace l
    match e with
    | Num(x) -> printfn "%d" x
    | Var(x) -> printfn "%s" x 
    | Array(s,e) -> printfn "%s[" s
                    printExpr e (l+1)
                    printSpace l
                    printfn "]"
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


let rec printB b l =
    printSpace l
    match b with
    | OrBool(b1,b2) -> printfn "|"
                       printB b1 (l+1)
                       printB b2 (l+1)
    | ScorBool(b1,b2) -> printfn "||"
                         printB b1 (l+1)
                         printB b2 (l+1)
    | AndBool(b1,b2) -> printfn "&"
                        printB b1 (l+1)
                        printB b2 (l+1)
    | ScandBool(b1,b2) -> printfn "&&"
                          printB b1 (l+1)
                          printB b2 (l+1)
    | NutBool(b1) -> printfn "!"
                     printB b1 (l+1)
    | EqualBool(e1,e2) -> printfn "="
                          printExpr e1 (l+1)
                          printExpr e2 (l+1)
    | NeqBool(e1,e2) -> printfn "!="
                        printExpr e1 (l+1)
                        printExpr e2 (l+1)
    | LtBool(e1,e2) ->  printfn "<"
                        printExpr e1 (l+1)
                        printExpr e2 (l+1)
    | GtBool(e1,e2) ->  printfn ">"
                        printExpr e1 (l+1)
                        printExpr e2 (l+1)
    | GeqBool(e1,e2) -> printfn ">="
                        printExpr e1 (l+1)
                        printExpr e2 (l+1)
    | LeqBool(e1,e2) -> printfn "<="
                        printExpr e1 (l+1)
                        printExpr e2 (l+1)
    | TrueBool -> printfn "true"
    | falseBool -> printfn "false"
        

let rec printCom c l =
    printSpace l
    match c with
    |AssCom(s,e) -> printfn ":="
                    printSpace (l+1)
                    printfn "%s" s
                    printExpr e (l+1)
    |AssArrayCom(s,e1,e2) -> printfn ":="
                             printSpace (l+1)
                             printfn "%s[" s
                             printExpr e1 (l+2)
                             printSpace (l+1)
                             printfn"]"
                             printExpr e2 (l+1);
    |SkipCom -> printfn "skip"
    |SemiCom(c1, c2) -> printfn ";"
                        printCom(c1) (l+1)
                        printCom(c2) (l+1)
    |IfCom(gc) -> printfn "if"
                  printGc gc (l+1)
                  printfn "fi"
    |DoCom(gc) -> printfn "do"
                  printGc gc (l+1)
                  printSpace l
                  printfn "od"
and printGc gc l =
    printSpace l
    match gc with
    |ArrowGc(b, c) -> printfn("->")
                      printB b (l+1)
                      printCom c (l+1)
    |IfElseGc(gc1, gc2) -> printfn("[]")
                           printGc gc1 (l+1)
                           printGc gc2 (l+1);



   

   
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
            | (PFlag, com) -> printCom com 0
                              printfn "Syntax is correct"
            | (det, com) -> makeNDGraph com det
        compute 1
        with err -> printfn "Syntax Wrong"
                    printfn "%s" err.Message
                    compute 1

// Start interacting with the user
compute 1
