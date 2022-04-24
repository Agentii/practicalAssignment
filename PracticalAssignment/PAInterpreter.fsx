module PAInterpreter

(*
#load "PATypesAST.fs"
open PATypesAST
#load "PACompiler.fsx"
open PACompiler
*)

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
    | GreaterThan(x, y) -> (evalAExp x mem) > (evalAExp y mem)
    | GreaterEqual(x, y) -> evalAExp x mem >= evalAExp y mem
    | LessThan(x, y) -> evalAExp x mem < evalAExp y mem
    | LessEqual(x, y) -> evalAExp x mem <= evalAExp y mem
    | ParB(x) -> evalBExp x mem

and powOf(x, y) =
    match y with
    | 0 -> x
    | _ -> powOf(x*x, y-1)

let memoryToString mem = Map.fold (fun e k v -> e + string(k) + ": " + string(v)) "" mem

let stepPrinter actionString nodeString memString = printfn "\nAction: %s\nNode: q%s\n%s" actionString nodeString memString

let rec findNextEdge currNode edges mem =
    match edges with
    | (q1, label, q2)::edges when currNode = q1 -> match label with
                                                   | AssignmentLabel(k, v) -> Some (q1, label, q2)
                                                   | SkipLabel -> Some (q1, label, q2)
                                                   | ConditionLabel(b) when evalBExp b mem -> Some (q1, label, q2)
                                                   | _ -> findNextEdge currNode edges mem
    | _::edges -> findNextEdge currNode edges mem
    | [] -> None

let getFinalNode (edges: (int*Label*int) list) =
    match edges.Item(edges.Length - 1) with
    | (_, _, q2) -> q2

let interpreter gcl =
    let edges = compile gcl
    let q0 = 0
    let qfinal = getFinalNode edges
    let mutable mem = initMemory ()
    printf "Enter allowed steps: "
    let allowedSteps = int (System.Console.ReadLine())
    stepPrinter ("") (string q0) (memoryToString(mem))

    let evalEdge edge =
        match edge with
        | (_, label, q2) -> let action = labelPrinter(label)
                            let node = string q2
                            let memory = match label with
                                            | AssignmentLabel(k, v) ->  let v = evalAExp v mem
                                                                        mem <- Map.add k v mem
                                                                        memoryToString(mem)
                                            | _ -> memoryToString(mem)
                                        
                            stepPrinter action node memory

    let rec interpreter node steps = 
        match steps with
        | steps when steps >= allowedSteps -> "\nMax allowed steps reached!\nProgram terminated...\n"
        | _ -> let nextEdge = findNextEdge node edges mem
               match nextEdge with
               | Some (q1, label, q2) -> evalEdge (q1, label, q2)
                                         if q2 = qfinal then ("\nSuccesfully terminated in " + string(steps+1) + " steps!") else interpreter q2 (steps+1)
               | None -> "Action: Stuck"
                
    interpreter q0 0     



// AssignmentLabel(string, a)
// SkipLabel
// ConditionLabel(b)