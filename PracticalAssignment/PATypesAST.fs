// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module PATypesAST

type a =
  | Num of float 
  | Var of string
  | Arr of (string * a)
  | Times of (a * a)
  | Div of (a * a)
  | Plus of (a * a)
  | Minus of (a * a)
  | Pow of (a * a)
  | UMinus of a
  | ParA of a

type b = 
  | Bool of bool
  | SCAnd of (b * b)
  | SCOr of (b * b)
  | And of (b * b)
  | Or of (b * b)
  | Not of (b)
  | Equal of (a * a)
  | NEqual of (a * a)
  | GreaterThan of (a * a)
  | GreaterEqual of (a * a)
  | LessThan of (a * a)
  | LessEqual of (a * a)
  | ParB of b

type C =
  | Ass of (string * a)
  | ArrAss of (a * a)
  | Skip
  | Seq of (C * C)
  | If of GC
  | Do of GC

and GC = 
  | Eval of (b * C)
  | Branch of (GC * GC)



