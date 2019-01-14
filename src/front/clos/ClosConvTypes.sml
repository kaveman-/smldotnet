structure ClosConvTypes =
struct

type ClosDef = 
  {
    fvtys : MILTy.Type list,    (* Upper bound on types of free variables *)
    meths : 
    {
      tyvars: (Var.Var*MILTy.Kind) list, (* Method might be polymorphic *)
      fvtys : MILTy.Type list,      (* Actual types for fvs for this method *)
      fundef: MILTerm.BoundVar * MILTerm.TAbstr * MILTy.CmpType
    } IntMap.map
  }

(*----------------------------------------------------------------------*)
(* The result type of closure conversion				*)
(*----------------------------------------------------------------------*)
type Result =
{
  (* Global function definitions with local blocks required *)
  fundefs : ((Var.Var*MILTy.Kind) list * MILTerm.BoundVar * MILTerm.TAbstr * MILTy.CmpType) Var.Map.map,

  (* Global variable types and names *)
  globvars : MILTerm.TypedVar Var.Map.map,

  (* User class definitions with local blocks required *)
  classdefs: 
    (MILTy.Type * MILTerm.ClassInfo * MILTerm.FieldInfo list * 
     MILTerm.MethodInfo list) list,

  (* Closure classes with local blocks required *)
  (* The i'th element of the list is closure class number i *)
  closdefs : ClosDef list,

  (* App methods required in top function class *)
  appmeths : int Var.Map.map,

  (* Types of app methods *)
  methtys : MILTy.Type list,

  (* The clinit term with local blocks required *)
  clinit : MILTerm.Cmp,

  bindeffects : Effect.Effect Var.Map.map

}

end
