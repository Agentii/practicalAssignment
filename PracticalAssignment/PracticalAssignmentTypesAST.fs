// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module PracticalAssignmentTypesAST

type a =
  | n of float 
  | x of string
  | A of (string * a)
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
  | Ass of (a * a)
  | skip
  | Seq of (C * C)
  | If of GC
  | Do of GC

and GC = 
  | Eval of (b * C)
  | Branch of (GC * GC)

