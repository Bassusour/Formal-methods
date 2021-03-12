//open System
//#load "GCLTypesAST.fs"
//open GCLTypesAST

let stateToString = function
    |Qs -> "qs"
    |Qf -> "qf"
    |Q(x) -> "q" + (string x);

let printLabel str q1 q2 = printfn "%s -> %s [label = \"%s\"];" (stateToString q1) (stateToString q2)  str; 

let rec allFalse gc = 
    match gc with
    |ArrowGc(b,c) -> NutBool(b)
    |IfElseGc(gc1,gc2) -> AndBool(allFalse gc1, allFalse gc2);
let nextState q n =
    match q with
    |Qs -> Q(n)
    |Q(x) -> Q(x+n)
    |Qf -> failwith("finalé staté")
let rec eToString  = function
    | Num(x) -> string x
    | Var(x) -> x 
    | Array(s,e) -> s + "[" + eToString e + "]"
    | TimesExpr(x,y) -> "(" + eToString x + "*" + eToString y + ")"
    | DivExpr(x,y) -> "(" + eToString x + "/" + eToString y + ")"
    | PlusExpr(x,y) -> "(" + eToString x + "+" + eToString y+")"
    | MinusExpr(x,y) -> "(" + eToString x + "-" + eToString y + ")"
    | PowExpr(x,y) -> "(" + eToString x + "^" + eToString y + ")"
    | UPlusExpr(x) -> "(" + "+" + eToString x + ")"
    | UMinusExpr(x) -> "("+ "-" + eToString x + ")"
let rec bToString b  =
    match b with 
      |OrBool(b1, b2) -> bToString b1 + "|" + bToString b2
      |ScorBool(b1, b2) -> bToString b1 + "||" + bToString b2
      |AndBool(b1, b2) -> bToString b1 + "&" + bToString b2
      |ScandBool(b1, b2) -> bToString b1 + "&&" + bToString b2
      |NutBool(b1) ->"!" + bToString b1
      |EqualBool(e1, e2) -> eToString e1 + "=" + eToString e2
      |NeqBool(e1, e2) -> eToString e1 + "!=" + eToString e2
      |LtBool(e1, e2) -> eToString e1 + "<" + eToString e2
      |GtBool(e1, e2) -> eToString e1 + ">" + eToString e2
      |GeqBool(e1, e2) -> eToString e1 + ">=" + eToString e2
      |LeqBool(e1, e2) -> eToString e1 + "<=" + eToString e2
      |TrueBool -> "true"
      |FalseBool -> "false"
let rec arrowC c q1 q2 n =
    match c with
        |AssCom(s,e) -> printLabel (s + ":=" + (eToString e)) q1 q2
                        n
        |AssArrayCom(s,e1,e2) -> printLabel (s+ "[" + eToString e1 + "]:=" + (eToString e2)) q1 q2
                                 n
        |SkipCom -> printLabel "skip" q1 q2
                    n
        |SemiCom(c1, c2) -> match q1 with
                            |Qs -> arrowC c2 (Q(1)) q2 (arrowC c1 Qs (Q(1)) n+1) 
                            |Q(x) -> arrowC c2 (Q(x+1)) q2 (arrowC c1 (Q(x)) (Q(x+1)) n+1)
                            
        |IfCom(gc) -> arrowGC gc q1 q2 n 
        |DoCom(gc) -> printLabel (bToString (allFalse gc)) q1 q2
                      arrowGC gc q1 q1 n 
                      
and arrowGC gc q1 q2 n  =
    match gc with
        |ArrowGc(b, c) -> arrowC c (Q(n+1)) q2 (arrowB b q1 (Q(n+1)) (n+1))
        |IfElseGc(gc1, gc2) ->  arrowGC gc2 q1 q2 (arrowGC gc1 q1 q2 (n+1))
and arrowB b q1 q2 n = printLabel (bToString b) q1 q2
                       n;

let makeNDGraph c =
    printfn "digraph program_graph {rankdir=LR;"
    printfn "node [shape = circle]; qs;"
    printfn "node [shape = doublecircle]; qf;"
    printfn "node [shape = circle]"
    arrowC c Qs Qf 0
    printfn "}";