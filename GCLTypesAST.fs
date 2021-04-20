// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module GCLTypesAST

type Sign =
    |Zero
    |Plus
    |Minus

type expr = 
  | Num of int
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | Var of string
  | Array of (string * expr)


type boolExpr =
  |OrBool of (boolExpr * boolExpr)
  |ScorBool of (boolExpr * boolExpr)
  |AndBool of (boolExpr* boolExpr)
  |ScandBool of (boolExpr* boolExpr)
  |NutBool of (boolExpr)
  |EqualBool of (expr * expr)
  |NeqBool of (expr * expr)
  |LtBool of (expr * expr)
  |GtBool of (expr * expr)
  |GeqBool of (expr * expr)
  |LeqBool of (expr * expr)
  |TrueBool
  |FalseBool

type com =
  |AssCom of (string * expr)
  |AssArrayCom of (string * expr * expr)
  |SkipCom
  |SemiCom of (com * com)
  |IfCom of gc
  |DoCom of gc
and gc =
  |ArrowGc of (boolExpr * com)
  |IfElseGc of (gc * gc)


type flag = 
  |DFlag  
  |NDFlag  
  |PFlag  
  |StepFlag
  |SAFlag

type state =
  |Qs
  |Qf
  |Q of (int)

 exception StuckException of (state* Map<string, int>* Map<string, int[]>)