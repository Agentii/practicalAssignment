module PAInterpreter

(*
#load "PATypesAST.fs"
open PATypesAST
#load "PACompiler.fsx"
open PACompiler
*)

let stepPrinter action node memory =
    let memoryString = Map.fold (fun e k v -> e + string(k) + ": " + string(v) + "\n") "" memory
    "\nAction: " + action + "\nNode: " + string(node) + "\n" + memoryString

let initMemory () =
    let mutable memory = Map []
    printf "Initialize variables: "
    let input = System.Console.ReadLine()
    let initArr = input.Split [|','|]
    for elem in initArr do
        let kvArr = elem.Split [|'='|]
        printfn "%A" kvArr
        memory <- Map.add (kvArr[0]) (int kvArr[1]) memory
        printfn "%s %d" (kvArr[0]) (int kvArr[1])
        printfn "%s" (Map.fold (fun e k v -> e + string(k) + ": " + string(v) + "\n") "" memory)
    memory

let rec evalAExp e mem =
  match e with
    | Num(x) -> int x
    | Var(x) -> 1 //Map.find x mem
    | Arr(x, y) -> 1
    | Times(x,y) -> evalAExp x mem * evalAExp y mem
    | Div(x,y) -> evalAExp x mem / evalAExp y mem
    | Plus(x,y) -> evalAExp x mem + evalAExp y mem
    | Minus(x,y) -> evalAExp x mem - evalAExp y mem
    | Pow(x,y) -> powOf(evalAExp x mem, evalAExp y mem)
    | UMinus(x) -> -evalAExp x mem
    | ParA(x) -> evalAExp x mem

and powOf(x, y) =
    match y with
    | 0 -> x
    | _ -> powOf(x*x, y-1)

let interpreter gcl =
    let edges = compile gcl
    let q0 = 0
    let mem = initMemory ()
    printfn "%s" (stepPrinter "" q0 mem)
    let rec interpreter edges mem = 
        match edges with
        | (_, label, q2)::edges -> let action = labelPrinter(label)
                                   match label with
                                                | AssignmentLabel(k, v) ->  let v = evalAExp v mem
                                                                            Map.add k v mem
                                                | SkipLabel -> mem
                                                | ConditionLabel(_) -> mem
                                   (stepPrinter action q2 mem) + (interpreter edges mem)
        | [] -> ""
    interpreter edges mem



// AssignmentLabel(string, a)
// SkipLabel
// ConditionLabel(b)