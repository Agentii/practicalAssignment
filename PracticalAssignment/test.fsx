module PAInterpreter

(*
#load "PATypesAST.fs"
open PATypesAST
#load "PACompiler.fsx"
open PACompiler
*)

let stepPrinter action node memory stuck =
    match stuck with
    | true -> ""
    | false -> let memoryString = Map.fold (fun e k v -> e + string(k) + ": " + string(v) + "\n") "" memory
               "\nAction: " + action + "\nNode: q" + string(node) + "\n" + memoryString

let initMemory () =
    let mutable memory = Map []
    printf "Initialize variables: "
    let input = System.Console.ReadLine()
    let initArr = input.Split [|','|]
    for elem in initArr do
        let kvArr = elem.Split [|'='|]
        memory <- Map.add (String.filter (fun x -> x <> ' ') kvArr[0]) (int kvArr[1]) memory
    memory

let rec evalAExp a mem =
  match a with
    | Num(x) -> int x
    | Var(x) -> Map.find x mem
    | Arr(x, y) -> 1
    | Times(x,y) -> evalAExp x mem * evalAExp y mem
    | Div(x,y) -> evalAExp x mem / evalAExp y mem
    | Plus(x,y) -> evalAExp x mem + evalAExp y mem
    | Minus(x,y) -> evalAExp x mem - evalAExp y mem
    | Pow(x,y) -> powOf(evalAExp x mem, evalAExp y mem)
    | UMinus(x) -> -evalAExp x mem
    | ParA(x) -> evalAExp x mem

and evalBExp b mem =
    match b with
    | Bool(x) -> x
    | SCAnd(x, y) -> evalBExp x mem && evalBExp y mem
    | SCOr(x, y) -> evalBExp x mem || evalBExp y mem
    | And(x, y) -> if evalBExp x mem then evalBExp y mem else evalBExp y mem && false
    | Or(x, y) -> if evalBExp x mem then evalBExp y mem || true else evalBExp y mem
    | Not(x) -> not(evalBExp x mem)
    | Equal(x, y) -> evalAExp x mem = evalAExp y mem
    | NEqual(x, y) -> evalAExp x mem <> evalAExp y mem
    | GreaterThan(x, y) -> evalAExp x mem > evalAExp y mem
    | GreaterEqual(x, y) -> evalAExp x mem >= evalAExp y mem
    | LessThan(x, y) -> evalAExp x mem < evalAExp y mem
    | LessEqual(x, y) -> evalAExp x mem <= evalAExp y mem
    | ParB(x) -> evalBExp x mem

and powOf(x, y) =
    match y with
    | 0 -> x
    | _ -> powOf(x*x, y-1)

let interpreter gcl =
    let edges = compile gcl
    let q0 = 0
    let mutable mem = initMemory ()
    printf "Enter allowed steps: "
    let allowedSteps = int (System.Console.ReadLine())
    printfn "%s" (stepPrinter "" q0 mem false)
    let rec interpreter edges node steps stuck = 
        match steps with
        | steps when steps >= allowedSteps -> "\nMax allowed steps reached!\nProgram terminated...\n"
        | _ -> let goodEdges = executable(edges,node)
        // check if goodEdges is empty
               let edge = chooose(goodEdges)
               let newnode = nextNodeAndeMemory(edge)
               interpreter(edges,newnode)

let compute nextNoodeAndMemory edge     
       match edge with
        ((node, label, q2)) -> let action = labelPrinter(label)
                                        match label with
                                        | AssignmentLabel(k, v) ->  let v = evalAExp v mem
                                                                    mem <- Map.add k v mem
                                                                    (stepPrinter action q2 mem stuck) + (interpreter edges q2 (steps+1) stuck)
                                        | SkipLabel -> false
                                        | ConditionLabel(b)-> not(evalBExp b mem)
                                        
                                   
        | ([], _) -> "\nAction: Successfully terminated in " + string(steps) + " steps."
    interpreter edges 0 false



// AssignmentLabel(string, a)
// SkipLabel
// ConditionLabel(b)