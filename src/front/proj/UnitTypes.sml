(*======================================================================*)
(* Type defs for units of compilation.					*)
(*======================================================================*)
structure UnitTypes =
struct

type StrEntry =
  {
    (* The SML environment to which the structure elaborates *)
    E : Env.Env,

    (* The MIL variable supply *)
    supply : Var.Supply,

    (* The MIL term-bound type variable supply *)
    tyvarsupply : Var.Supply,

    (* The MIL term expressed as an abstraction \<S1,...,Sn>.e where the
       symbolic names of the bound variables are the imported structures *)
    term : MILTerm.Abstr,

    (* MIL type defs for this module, assumed to be closed wrt tynames
       other than those that are imported and abstract *)
    tynameTys : MILTy.Type TyName.Map.map

  }

datatype Entry = 
  Sig of Env.Sig
| Fun of Env.FunInfo
| Str of StrEntry

end (* of struct *)

