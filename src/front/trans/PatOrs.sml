(*======================================================================*)
(* Compilation of or-patterns.						*)
(*======================================================================*)
structure PatOrs =
struct

(*----------------------------------------------------------------------*)
(* Preprocess a list of rules, removing or-patterns by duplicating	*)
(* clauses. Hopefully pattern compilation will do a good job!		*)
(*----------------------------------------------------------------------*)
local open SMLTerm in
fun translate pats =
let
  fun process rhs pat =
  case pat of
    PatOr pats =>
    List.concat (map (process rhs) pats)

(* SL: or *)
(*
  | (PatVar _ | PatWild | PatSCon _ | PatCast _ | 
     PatCon(_,_,_,NONE) | PatExCon(_,NONE) | PatLiteral _) =>
    [pat]
*)
  | PatVar _ => [pat]
  | PatWild => [pat]
  | PatSCon _ => [pat]
  | PatCast _ => [pat]
  | PatCon(_,_,_,NONE) => [pat]
  | PatExCon(_,NONE) => [pat]
  | PatLiteral _ => [pat]

  | PatRef pat =>     
    map PatRef (process rhs pat)

  | PatCon(id,def,ty,SOME pat) =>
    map (fn pat => PatCon(id,def,ty,SOME pat)) (process rhs pat)

  | PatExCon(id,SOME (ty,pat)) =>
    map (fn pat => PatExCon(id,SOME (ty,pat))) (process rhs pat)

  | PatLayer(id, pat) =>
    map (fn pat => PatLayer(id, pat)) (process rhs pat)

  | PatRecord(b, patrow) =>
    let
      fun over [] = [[]]
        | over ((lab,pat)::patrow) =
          let
            val pats = process rhs pat
            val patrows = over patrow
          in
            List.concat (map (fn pat => 
              map (fn patrow => (lab,pat)::patrow) patrows) pats)
          end
    in
      map (fn patrow => PatRecord(b, patrow)) (over patrow)
    end

  | PatVec pats =>
    Debug.fail "PatOrs.preprocess: vector pattern"
in
  List.concat (ListOps.mapi (fn (rhs,pat) => let val pats = process rhs pat
    in map (fn pat => (pat, rhs)) pats end) pats)
end (* of let *)
end (* of local open *)

end (* of structure *)