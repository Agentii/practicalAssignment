#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "PATypesAST.fs"
open PATypesAST
#load "PAParser.fs"
open PAParser
#load "PALexer.fs"
open PALexer


let rec evalA e =
  match e with
    | Num(x) -> string x
    | Var(x) -> x
    | Arr(x, y) -> x + "[" + evalA(y) + "]"
    | Times(x,y) -> evalA(x) + " * " + evalA(y)
    | Div(x,y) -> evalA(x) + " / " + evalA(y)
    | Plus(x,y) -> evalA(x) + "+" + evalA(y)
    | Minus(x,y) -> evalA(x) + "-" + evalA(y)
    | Pow(x,y) -> evalA(x) + " ** " + evalA(y)
    | UMinus(x) -> "-" + evalA(x)
    | ParA(x) -> "(" + evalA(x) + ")"

let rec evalB e = 
  match e with
    | Bool(true) -> "true"
    | Bool(false) -> "false"
    | SCAnd(x, y) -> evalB(x) + " & " + evalB(y)
    | SCOr(x, y) -> evalB(x) + " | " + evalB(y)
    | And(x, y) -> evalB(x) + " && " + evalB(y)
    | Or(x, y) -> evalB(x) + " || " + evalB(y)
    | Not(x) -> "!" + evalB(x)
    | Equal(x, y) -> evalA(x) + " = " + evalA(y)
    | NEqual(x, y) -> evalA(x) + " != " + evalA(y)
    | GreaterThan(x, y) -> evalA(x) + " > " + evalA(y)
    | GreaterEqual(x, y) -> evalA(x) + " >= " + evalA(y)
    | LessThan(x, y) -> evalA(x) + " < " + evalA(y)
    | LessEqual(x, y) -> evalA(x) + " <= " + evalA(y)
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

#load "PACompiler.fsx"
open PACompiler
#load "PAInterpreter.fsx"
open PAInterpreter

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = PAParser.start PALexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "\nBYE BYE BABY!"
    else
        printf "\nEnter a GCL expression: "
        //try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "\nInput parsed as: %s" (evalC(e))
        
        let res = compile e
        printfn "\nList of edges:\n%A" res
        printfn "\n%s" (graphvizPrinter(res))

        printfn "\nStarting interpreter...\n"
        printfn "%s" (interpreter e)

        compute n
        //with error -> printfn "Error: Wrong input"
        //              compute (n-1)


// Start interacting with the user
compute 3  // 3 is the number of allowed failures

// y:= 1; x:=2; z:=4
// x:=1; y:=1; z:=1; r:=1; s:=1
// x:=2+2; if x>2 -> x:=1 [] x=2 -> x:=0 [] x<2 -> x:=-1 fi
// x:=2+2; if x>2 -> x:=1 [] x=2 -> do x<0 -> x:=1 od [] x<2 -> x:=-1 fi
// x:=(x+1)*2; if x>6 -> x:=6 [] x=6 -> do x>0 -> x:=x-1 od [] x<6 -> x:=-6 fi