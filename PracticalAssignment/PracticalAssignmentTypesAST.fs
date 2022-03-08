// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module PracticalAssignmentTypesAST


// TA? Is any of this correct?
type a =
  | n of float 
  | x of string
  | A of array<a>
  | Times of (a * a)
  | Div of (a * a)
  | Plus of (a * a)
  | Minus of (a * a)
  | Pow of (a * a)
  | UMinus of a
  | Par of a

type b = 
  | true | false
  | SCAnd of (b * b)
  | SCOr of (b * b)
  | And of (b * b)
  | Or of (b * b)
  | Not of (b * b)
  | EQ of (b * b)
  | NEQ of (b * b)
  | GT of (b * b)
  | GEQ of (b * b)
  | LT of (b * b)
  | LEQ of (b * b)
  | Par of b

type C =
  | Ass of (a * a)
  | skip
  | Branch of (C * C)
  | If of GC
  | Do of GC

type GC = 
  | Eval of (b * C)
  | Branch of (GC * GC)

