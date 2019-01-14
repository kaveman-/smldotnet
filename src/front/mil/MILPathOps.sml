(*======================================================================*)
(* Path operations.                                                     *)
(*======================================================================*)
structure MILPathOps :> MILPATHOPS =
struct

local 
  open MILPath
in

(*----------------------------------------------------------------------*)
(* Can we hoist through this scope?					*)
(*----------------------------------------------------------------------*)
fun canHoistThrough 
    (MILPath.LetFun { var, recursive = false, 
      kind = SOME MILTerm.LocalFun }) = true
  | canHoistThrough (MILPath.LetFun _) = false
  | canHoistThrough _ = true


(*----------------------------------------------------------------------*)
(* Pretty-print a path.  						*)
(*----------------------------------------------------------------------*)
local
  fun itemToString (LetFun { var, kind, ... }) = 
      (case kind of NONE => "method " | SOME MILTerm.LocalFun => "block " 
          | SOME MILTerm.KnownFun => "global " | _ => "closure ") ^ Var.toString var
    | itemToString (CaseCon iopt) = 
      (case iopt of NONE => "_" | SOME i => Int.toString i)
    | itemToString (CaseSCon copt) = 
      (case copt of NONE => "_" | SOME c => Constants.constant_toString c)
    | itemToString (TypeCase eopt) = 
      (case eopt of NONE => "_" | SOME e => MILTy.toString e)
in
  fun toString items = Pretty.simpleVec "/" itemToString (rev items)
end

fun eqItem (LetFun { var = f, ...}, LetFun { var = g, ... }) = Var.eq(f,g)
  | eqItem (CaseCon i1, CaseCon i2) = i1=i2
  | eqItem (CaseSCon i1, CaseSCon i2) = 
    Eq.option (fn (c1,c2) => Constants.equal (c1,c2,false)) (i1,i2)
  | eqItem (TypeCase e1, TypeCase e2) = Eq.option MILTy.eq (e1,e2)
  | eqItem _ = false

(*----------------------------------------------------------------------*)
(* Equality on paths							*)
(*----------------------------------------------------------------------*)
fun eq (path1, path2) = Eq.list eqItem (path1,path2)

(*----------------------------------------------------------------------*)
(* Largest common suffix						*)
(*----------------------------------------------------------------------*)
fun join (path1, path2) =    
let
  fun loop prefix (item1::items1, item2::items2) =
      if eqItem (item1,item2) then loop (item1::prefix) (items1,items2)
      else prefix
    | loop prefix _ = prefix
in
  loop [] (rev path1,rev path2)
end

structure Map =
struct

(*----------------------------------------------------------------------*)
(* Datatype for trees, used for mapping paths to scopes.		*)
(*----------------------------------------------------------------------*)
datatype 'a Branch = 
  LetFunNode of 'a map Var.Map.map * 'a Branch
| CaseConNode of 'a map IntMap.map * 'a map 
| CaseSConNode of 'a map CMap.map * 'a map 
| TypeCaseNode of 'a map MILTy.Map.map * 'a map
| Empty

withtype 'a map = 'a list * 'a Branch

val empty = ([], Empty)

fun treeToString depth f (a, br) =
Pretty.newline depth ^ "[" ^ Pretty.simpleVec "," f a ^ "]" ^ 
Pretty.newline depth ^ branchToString depth f br  

and branchToString depth f br =
case br of
  LetFunNode(funs,body) => 
  "letfun " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "and ")
  (fn (x,tree) => Var.toString x ^ ":" ^ treeToString (depth+1) f tree)
  (Var.Map.listItemsi funs) ^ Pretty.newline depth ^ "in " ^ 
    branchToString (depth+1) f body

| CaseConNode(cases,default) => 
  "caseof " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "| ") 
  (fn (i,tree) => Int.toString i ^ ":" ^ treeToString (depth+1) f tree)
  (IntMap.listItemsi cases) ^ 
  (Pretty.newline depth ^ "| _ => " ^ treeToString (depth+1) f default)

| CaseSConNode(cases,default) => 
  "scase of " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "| ")
  (fn (c,tree) => Constants.constant_toString c ^ ":" ^ 
    treeToString (depth+1) f tree)
  (CMap.listItemsi cases) ^ 
  (Pretty.newline depth ^ "| _ => " ^ treeToString (depth+1) f default)

| TypeCaseNode(cases,default) =>
  "excase of " ^ 
  Pretty.simpleVec (Pretty.newline depth ^ "| ")
  (fn (e,tree) => "?:" ^ treeToString (depth+1) f tree)
  (MILTy.Map.listItemsi cases) ^ 
  (Pretty.newline depth ^ "| _ => " ^ treeToString (depth+1) f default)

| Empty => ""

local
fun find' ((a,_), []) = a
  | find' ((a,br), path) =

    case (br,path) of
      (LetFunNode(funs,body), LetFun { var = f, ... } :: path) =>
      (case Var.Map.find(funs, f) of
        NONE => []
      | SOME tree => find' (tree, path)
      )
    | (LetFunNode(funs,body), path) => find' (([],body), path)

    | (CaseConNode(cases, default), CaseCon iopt :: path) =>
      (case iopt of
        NONE => find' (default, path)
      | SOME i =>
        case IntMap.find(cases, i) of
          NONE => []
        | SOME tree => find' (tree, path)
      )

    | (CaseSConNode(cases, default), CaseSCon copt :: path) =>
      (case copt of
        NONE => find' (default, path)
      | SOME c =>
        case CMap.find(cases, c) of
          NONE => []
        | SOME tree => find' (tree, path)
      )

    | (TypeCaseNode(cases, default), TypeCase eopt :: path) =>
      (case eopt of
        NONE => find' (default, path)
      | SOME e =>
        case MILTy.Map.find(cases, e) of
          NONE => []
        | SOME tree => find' (tree, path)
      )

    | _ => []
in
  fun find (tree, path) = find' (tree, rev path)
end
  
fun insert (tree, patharg, item) =
let
  fun create (a,[]) = 
      (item :: a, Empty)

    | create (a, LetFun { var = f, ... } :: path) = 
      (a, LetFunNode(Var.Map.insert(Var.Map.empty, f, create' path), Empty))

    | create (a, CaseCon NONE :: path) = 
      (a, CaseConNode(IntMap.empty, create' path))
    | create (a, CaseCon (SOME i) :: path) = 
      (a, CaseConNode(IntMap.insert(IntMap.empty, i, create' path), empty))
     
    | create (a, CaseSCon NONE :: path) = 
      (a, CaseSConNode(CMap.empty, create' path))
    | create (a, CaseSCon (SOME c) :: path) = 
      (a, CaseSConNode(CMap.insert(CMap.empty, c, create' path), empty))
     
    | create (a, TypeCase NONE :: path) = 
      (a, TypeCaseNode(MILTy.Map.empty, create' path))
    | create (a, TypeCase (SOME e) :: path) = 
      (a, TypeCaseNode(MILTy.Map.insert(MILTy.Map.empty, e, create' path), 
        empty))

  and create' path = create ([], path)
     
  fun insert' ((a,br), []) = (item::a, br)
    | insert' ((a,Empty), path) = create (a,path)
    | insert' ((a,br), path) = 
      let 
      fun insertBr (br,path) =
      case (br,path) of

      (Empty, path) =>
      #2 (create' path)

    | (LetFunNode(funs, body), LetFun {var = f, ... } :: path) =>
      (case Var.Map.find(funs, f) of
        NONE => 
        LetFunNode(Var.Map.insert(funs, f, create' path), body)

      | SOME tree => 
        LetFunNode(Var.Map.insert(funs, f, insert' (tree, path)), body)
      )

    | (LetFunNode(funs, body), path) =>
      LetFunNode(funs, insertBr(body,path))

    | (_, LetFun { var = f, ... } :: path) =>
      LetFunNode(Var.Map.insert(Var.Map.empty, f, create' path), br)

    | (CaseConNode(cases, default), CaseCon iopt :: path) =>
      (case iopt of
        NONE => CaseConNode(cases, insert' (default, path))
      | SOME i =>
        case IntMap.find(cases, i) of
          NONE => CaseConNode(IntMap.insert(cases, i, create' path), default)
        | SOME tree => 
          CaseConNode(IntMap.insert(cases, i, insert' (tree, path)), default)
      )

    | (CaseSConNode(cases, default), CaseSCon copt :: path) =>
      (case copt of
        NONE => CaseSConNode(cases, insert' (default, path))
      | SOME c =>
        case CMap.find(cases, c) of
          NONE => CaseSConNode(CMap.insert(cases, c, create' path), default)
        | SOME tree => 
          CaseSConNode(CMap.insert(cases, c, insert' (tree, path)), default)
      )


(* The problem with pattern matching is in here somewhere! *)
    | (TypeCaseNode(cases, default), TypeCase eopt :: path) =>
      (case eopt of
        NONE => TypeCaseNode(cases, insert' (default, path))
      | SOME e =>
        case MILTy.Map.find(cases, e) of
          NONE => 
          TypeCaseNode(MILTy.Map.insert(cases, e, create' path), default)
        | SOME tree => 
          TypeCaseNode(MILTy.Map.insert(cases, e, insert'(tree,path)),default)
      ) 

     | (br, path) =>
      Debug.fail ("MILPathOps.Map.insert: match failure with\ntree = " 
       ^ treeToString 0 (fn _ => "*") tree ^ "\npath = " ^ toString patharg)

   in (a,insertBr (br,path)) end


in
  insert' (tree, rev patharg)
end

fun toString f = treeToString 0 f


end (* of structure Map *)
  

end (* of local open *)

end (* of struct *)

