// This file implements a module where we define a data type "Expr"
// to store represent arithmetic Expressions
module GCLTypesAST

type Sign =
    |Zero
    |Plus
    |Minus

type Expr = 
  | Num of int
  | TimesExpr of (Expr * Expr)
  | DivExpr of (Expr * Expr)
  | PlusExpr of (Expr * Expr)
  | MinusExpr of (Expr * Expr)
  | PowExpr of (Expr * Expr)
  | UPlusExpr of (Expr)
  | UMinusExpr of (Expr)
  | Var of string
  | Array of (string * Expr)


type BoolExpr =
  |OrBool of (BoolExpr * BoolExpr)
  |ScorBool of (BoolExpr * BoolExpr)
  |AndBool of (BoolExpr* BoolExpr)
  |ScandBool of (BoolExpr* BoolExpr)
  |NutBool of (BoolExpr)
  |EqualBool of (Expr * Expr)
  |NeqBool of (Expr * Expr)
  |LtBool of (Expr * Expr)
  |GtBool of (Expr * Expr)
  |GeqBool of (Expr * Expr)
  |LeqBool of (Expr * Expr)
  |TrueBool
  |FalseBool

type Com =
  |AssCom of (string * Expr)
  |AssArrayCom of (string * Expr * Expr)
  |SkipCom
  |SemiCom of (Com * Com)
  |IfCom of gc
  |DoCom of gc
and gc =
  |ArrowGc of (BoolExpr * Com)
  |IfElseGc of (gc * gc)


type flag = 
  |DFlag  
  |NDFlag  
  |PFlag  
  |StepFlag
  |SAFlag
  |SECFlag
  |FlemmingFlag

type state =
  |Qs
  |Qf
  |Q of (int)

 exception StuckException of (state* Map<string, int>* Map<string, int[]>)