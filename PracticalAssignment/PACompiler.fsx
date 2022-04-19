module PACompiler

(*
#load "PATypesAST.fs"
open PATypesAST
*)

type label =
    | AssignmentLabel of (string * a)
    | SkipLabel
    | ConditionLabel of b

type Edge = (int * label * int)


let rec depthCount C e = 
    match C with
    | Seq(c1, c2) -> let dc1 = depthCount c1 e 
                     depthCount c2 dc1
    | If(gc) -> depthCountGC gc e
    | Do(gc) -> depthCountGC gc e
    | _ -> e + 1

and depthCountGC gc e =
    match gc with
    | Eval(_, c) -> depthCount c (e+1)
    | Branch(gc1, gc2) -> let dc1 = depthCountGC gc1 e
                          depthCountGC gc2 dc1 - 1

let rec doneGC gc =
    match gc with
    | Eval(b, _) -> Not(b)
    | Branch(gc1, gc2) -> And(doneGC(gc1), doneGC(gc2))

let detCondition b bList = 
    match bList with
    | [] -> b
    | [x] -> And(b, Not(x))
    | e::bs -> And(b, Not(List.fold (fun e x -> Or(x, e)) e bs))

let rec genBList gc bList =
    match gc with
    | Eval(b, _) -> b::bList
    | Branch(gc1, gc2) -> genBList gc1 bList @ genBList gc2 []

let compile inp det =
    let q2 = depthCount inp 0
    let rec compile inp q1 q2 =
        match inp with
        | Skip -> [Edge(q1, SkipLabel, q2)]
        | Ass(x, y) -> [Edge(q1, AssignmentLabel(x, y), q2)]
        | Seq(c1, c2) -> let q = depthCount c1 q1
                         (compile c1 q1 q) @ (compile c2 q q2)
        | If(gc) -> if det then compileDetGC gc q1 q2 (q1+1) ([Bool(false)]) else compileGC gc q1 q2 (q1+1)
        | Do(gc) -> let b = doneGC(gc)
                    if det then (compileDetGC gc q1 q1 (q1+1) ([Bool(false)])) else (compileGC gc q1 q1 (q1+1)) @ [Edge(q1, ConditionLabel(b), q2)]
        | _ -> failwith ("Compile error!")
    
    and compileGC gc q1 q2 q =
        match gc with
        | Eval(b, c) -> [Edge(q1, ConditionLabel(b), q)] @ (compile c q q2)
        | Branch(gc1, gc2) -> let qn = depthCountGC gc1 q - 1
                              (compileGC gc1 q1 q2 q) @ (compileGC gc2 q1 q2 qn)
    
    and compileDetGC gc q1 q2 q bList =
        let mutable bList = []
        match gc with
        | Eval(b, c) -> [Edge(q1, ConditionLabel(detCondition b bList), q)] @ (compile c q q2)
        | Branch(gc1, gc2) -> let qn = depthCountGC gc1 q - 1
                              (compileDetGC gc1 q1 q2 q bList) @ (compileDetGC gc2 q1 q2 qn (genBList gc1 bList))
    compile inp 0 q2


let labelPrinter label =
    match label with
    | AssignmentLabel(name, a) -> name + ":=" + evalA(a)
    | SkipLabel -> "skip"
    | ConditionLabel(b) -> evalB(b)

let rec graphvizConverter pg = 
    match pg with
    | [] -> ""
    | [(q1, label, q2)] -> "q" + string(q1) + "->" + "q" + string(q2) + " [label=\"" + labelPrinter(label) + "\"]"
    | (q1, label, q2)::pgs -> "q" + string(q1) + "->" + "q" + string(q2) + " [label=\"" + labelPrinter(label) + "\"]\n " + graphvizConverter(pgs) 
    
let graphvizPrinter pg = "digraph G {\n " + graphvizConverter(pg) + "\n}"
   

// y:= 1; x:=2; z:=4
// x:=1; y:=1; z:=1; r:=1; s:=1
// x:=2+2; if x>2 -> x:=1 [] x=2 -> x:=0 [] x<2 -> x:=-1 fi
// x:=2+2; if x>2 -> x:=1 [] x=2 -> do x<0 -> x:=1 od [] x<2 -> x:=-1 fi


// x:=2+2; if x>2 -> x:=1 [] x=2 AND NOT x>2 -> x:=0 [] x<2 AND NOT x>2 AND NOT x=2 -> x:=-1 fi