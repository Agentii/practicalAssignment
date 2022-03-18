module PracticalAssignmentCompiler

//#load "PracticalAssignmentTypesAST.fs"
//open PracticalAssignmentTypesAST


type label =
    | AssignmentLabel of (string * a)
    | SkipLabel
    | ConditionLabel of b

type Edge = (int * label * int)
 

let depthCount C n =
    let rec depthCount C e = 
        match C with
        | Seq(c1, c2) -> depthCount c1 0 + depthCount c2 0 + 1
        | If(gc) -> depthCountGC gc 0 + 1
        | Do(gc) -> depthCountGC gc 0 + 1
        | _ -> e + 1
    
    and depthCountGC gc e =
        match gc with
        | Eval(_, c) -> e + depthCount c 0 + 1
        | Branch(gc1, gc2) -> depthCountGC gc1 0 + depthCountGC gc2 0 + 1
    
    depthCount C n

let compile inp =
    let rec compile q1 q2 inp n =
        match inp with
        | Skip -> [Edge(q1, SkipLabel, q2)]
        | Ass(x, y) -> [Edge(q1, AssignmentLabel(x, y), q2)]
        | Seq(c1, c2) -> let depth = depthCount c1 0
                         (compile q1 q2 c1 q2) @ (compile (q1+depth) (q1+depth+1) c2 (q1+depth+1))
        | If(Eval(b, c)) -> [Edge(q1, ConditionLabel(b), q2)] @ compile q2 (q2+1) c (q2+1)
        | If(Branch(gc1, gc2)) -> (compile q1 q2 gc1 q2) @ (compile q1 (n+1) gc2 q2)
        | Do(gc) -> []
        | _ -> failwith ("Compile error!")

    compile 0 1 inp 0

// y:= 1; x:=2; z:=4
