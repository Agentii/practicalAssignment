// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "PracticalAssignmentTypesAST.fs"
open PracticalAssignmentTypesAST
#load "PracticalAssignmentParser.fs"
open PracticalAssignmentParser
#load "PracticalAssignmentLexer.fs"
open PracticalAssignmentLexer

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)


let rec evalA e =
  match e with
    | Num(x) -> string x
    | Var(x) -> x
    | Arr(x, y) -> x + "[" + evalA(y) + "]"
    | Times(x,y) -> evalA(x) + "*" + evalA(y)
    | Div(x,y) -> evalA(x) + "/" + evalA(y)
    | Plus(x,y) -> evalA(x) + "+" + evalA(y)
    | Minus(x,y) -> evalA(x) + "-" + evalA(y)
    | Pow(x,y) -> evalA(x) + "**" + evalA(y)
    | UMinus(x) -> "-" + evalA(x)
    | ParA(x) -> "(" + evalA(x) + ")"

let rec evalB e = 
  match e with
    | Bool(true) -> "true"
    | Bool(false) -> "false"
    | SCAnd(x, y) -> evalB(x) + "&" + evalB(y)
    | SCOr(x, y) -> evalB(x) + "|" + evalB(y)
    | And(x, y) -> evalB(x) + "&&" + evalB(y)
    | Or(x, y) -> evalB(x) + "||" + evalB(y)
    | Not(x) -> "!" + evalB(x)
    | Equal(x, y) -> evalA(x) + "=" + evalA(y)
    | NEqual(x, y) -> evalA(x) + "!=" + evalA(y)
    | GreaterThan(x, y) -> evalA(x) + ">" + evalA(y)
    | GreaterEqual(x, y) -> evalA(x) + ">=" + evalA(y)
    | LessThan(x, y) -> evalA(x) + "<" + evalA(y)
    | LessEqual(x, y) -> evalA(x) + "<=" + evalA(y)
    | ParB(x) -> "(" + evalB(x) + ")"

let rec evalC e = 
  match e with
    | Ass(x, y) -> x + " := " + evalA(y)
    | ArrAss(x, y) -> evalA(x) + " := " + evalA(y)
    | Seq(x, y) -> evalC(x) + " ; " + evalC(y)
    | If(x) -> "if " + evalGC(x) + " fi"
    | Do(x) -> "do " + evalGC(x) + " od"
    | Skip -> "skip"

and evalGC e =
  match e with
    | Eval(x, y) -> evalB(x) + " -> " + evalC(y)
    | Branch(x, y) -> evalGC(x) + " [] " + evalGC(y)

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = PracticalAssignmentParser.start PracticalAssignmentLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        //try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: %s" (evalC(e))
        compute n
        //with err -> compute (n-1)

// Start interacting with the user
compute 3
