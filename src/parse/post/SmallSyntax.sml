(*======================================================================*)
(* Abstract syntax tree datatypes for compressed version of source	*)
(* containing only information relevant to dependency analysis.         *)
(* Note:                                                                *)
(*   There is no distinction between exps, decs, and strdecs.           *)
(*======================================================================*)
structure SmallSyntax =
struct

(*----------------------------------------------------------------------*)
(* Structure expressions						*)
(*    struct <dec> end                                                  *)
(*    <longstrid>                                                       *)
(*    <strexp> : <sigexp>    |    <strexp> :> <sigexp>                  *)
(*    <funid> ( <strexp> )                                              *)
(*    let <dec> in <strexp> end                                         *)
(*----------------------------------------------------------------------*)
datatype StrExp =
  Struct of Dec
| Strid of Syntax.longid
| StrConstraint of StrExp * SigExp
| FunApp of Syntax.symbol * StrExp
| StrLet of Dec * StrExp

(*----------------------------------------------------------------------*)
(* Signature expressions						*)
(*    <strid>                                                           *)
(*----------------------------------------------------------------------*)
and SigExp =
  Sigid of Syntax.symbol
| SigSpec of Spec
| Where of SigExp * Mention

(*----------------------------------------------------------------------*)
(* Specification items							*)
(*    structure <strid> : <sigexp> and ... and <strid> : <sigexp>       *)
(*    include <sigexp>                                                  *)
(* or a reference to a <longstrid>.                                     *)
(*----------------------------------------------------------------------*)
and SpecItem =
  StructureDesc of (Syntax.symbol * SigExp) list
| Include of SigExp
| SpecMention of Mention

(*----------------------------------------------------------------------*)
(* Declaration items (also used for expressions and strdecs)		*)
(*    local <dec> in <dec> end                                          *)
(*    open <longstrid> ... <longstrid>                                  *)
(* or a reference to a <longstrid>                                      *)
(*----------------------------------------------------------------------*)
and DecItem =
  Local of Dec * Dec
| Open of Syntax.longid list
| Structure of (Syntax.symbol * StrExp) list
| Signature of (Syntax.symbol * SigExp) list
| Functor of (Syntax.symbol * Spec * StrExp) list
| Mention of Mention

withtype Spec = SpecItem list
and Dec = DecItem list
and Mention = Longid.Set.set (* without final identifiers *)

(*----------------------------------------------------------------------*)
(* Pretty-printing functions						*)
(*----------------------------------------------------------------------*)
fun decItemToString di =
case di of
  Local(d1,d2) => 
  "(local " ^ decToString d1 ^ " in " ^ decToString d2 ^ " end)" 

| Open longids =>
  "open " ^ Pretty.simpleVec " " Longid.toString longids

| Mention m =>
  "{" ^ Pretty.simpleVec "," Longid.toString (Longid.Set.listItems m) ^
  "}"

| Structure b =>
  "structure " ^ Pretty.simpleVec " ; " (fn (strid,strexp) =>
    Id.toString strid ^ " = " ^ strExpToString strexp)
    b

| Signature b =>
  "signature"
 
| Functor f =>
  "functor"

and decToString d = Pretty.simpleVec " ; " decItemToString d

and strExpToString strexp =
case strexp of
  Struct d => "struct " ^ decToString d
| Strid strid => Longid.toString strid
| StrConstraint (strexp, sigexp) => strExpToString strexp ^ ":"
| FunApp(funid, strexp) => 
  Id.toString funid ^ "(" ^ strExpToString strexp ^ ")"
| StrLet(d,strexp) =>
  "let " ^ decToString d ^ " in " ^ strExpToString strexp ^ " end"

structure Pickle =
struct
local 
  open Pickle 
in
  val mention = wrap (fn x => Longid.Set.addList(Longid.Set.empty,x),
                      Longid.Set.listItems) (list IdPickle.longid)

  val (specitem, sigexp) = fix2 (fn (specitem, sigexp) =>
  (alttag (fn StructureDesc _ => 0 | Include _ => 1 | SpecMention _ => 2)
   [
     wrap (StructureDesc, fn StructureDesc x => x) (list(pair(IdPickle.id,sigexp))),
     wrap (Include, fn Include x => x) sigexp,
     wrap (SpecMention, fn SpecMention x => x) mention
   ],
   alttag (fn Sigid _ => 0 | SigSpec _ => 1 | Where _ => 2)
   [
     wrap (Sigid, fn Sigid x => x) IdPickle.id,
     wrap (SigSpec, fn SigSpec x => x) (list specitem),
     wrap (Where, fn Where x => x) (pair (sigexp, mention))
   ]))

  val (decitem, strexp) = fix2 (fn (decitem, strexp) =>
  (alttag (fn Local _ => 0 | Open _ => 1 | Structure _ => 2 | Signature _ => 3 | Functor _ => 4 | Mention _ => 5)
   [
     wrap (Local, fn Local x => x) (pair (list decitem, list decitem)),
     wrap (Open, fn Open x => x) (list IdPickle.longid),
     wrap (Structure, fn Structure x => x) (list (pair (IdPickle.id, strexp))),
     wrap (Signature, fn Signature x => x) (list (pair (IdPickle.id, sigexp))),
     wrap (Functor, fn Functor x => x) 
       (list (triple (IdPickle.id, list specitem, strexp))),
     wrap (Mention, fn Mention x => x) mention
   ],     
   alttag (fn Struct _ => 0 | Strid _ => 1 | StrConstraint _ => 2 | FunApp _ => 3 | StrLet _ => 4)
   [
     wrap (Struct, fn Struct x => x) (list decitem),
     wrap (Strid, fn Strid x => x) IdPickle.longid,
     wrap (StrConstraint, fn StrConstraint x => x) (pair (strexp, sigexp)),
     wrap (FunApp, fn FunApp x => x) (pair (IdPickle.id, strexp)),
     wrap (StrLet, fn StrLet x => x) (pair (list decitem, strexp))
   ]))

  val dec = list decitem
end
end

end
