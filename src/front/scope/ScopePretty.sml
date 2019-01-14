structure ScopePretty =
struct

local 
  open ScopeTypes MILTerm
  val [showFloat,showHoist,showFunKinds] = 
  map (Controls.add false) 
  ["floathoist.showFloat","floathoist.showHoist","funscope.showFunKinds"]

  fun bindingToString (ValBind(x,v)) = 
      Var.toString (#1 x) ^ " = " (* ^ MILPretty.valToString v *)
    | bindingToString (CmpBind(xs,e)) =
      Pretty.vec ("<>", "", "", "<", ">", ",") (Var.toString o #1 o #1) xs ^
      " <= " (* ^ MILPretty.cmpToString e *)
    | bindingToString (FunBind(tyvars,kind,Fun(f,_))) =
      Var.toString (#1 f) ^ " = fn ..."
    | bindingToString (FunBind(tyvars,kind,RecFun defs)) =
      Pretty.simpleVec " and " (fn (f,_,_,_) => Var.toString (#1 f) ^ " = fn ...") 
      defs
in

  fun dump
  ({ funkinds, 
     floatedbindings, replacedbindings, hoistedbindings, ... } : Info) =

    (PrintManager.dump showFloat (fn prln =>
      (prln ("Floated bindings =" ^
      (Pretty.bigVec 0 
         (fn (f,bs) => Var.toString f ^ " <-| " ^ 
            Pretty.simpleVec "," bindingToString bs)
         (Var.Map.listItemsi floatedbindings)));
         prln ("Replaced bindings =" ^ 
      (Pretty.bigVec 0
         (fn (x,v) => Var.toString x ^ " = " (* ^ MILPretty.valToString v *))
         (Var.Map.listItemsi replacedbindings)))));

    PrintManager.dump showHoist (fn prln =>
      prln ("Hoisted bindings =" ^
      (MILPathOps.Map.toString bindingToString hoistedbindings)));

    PrintManager.dump showFunKinds (fn prln => (prln ("Localised functions = " ^ Pretty.simpleVec ","
      (Var.toString o #1) (Var.Map.listItemsi (Var.Map.filter
        (fn k => k=LocalFun) funkinds)));
          prln ("Globalised functions = " ^ Pretty.simpleVec ","
      (Var.toString o #1) (Var.Map.listItemsi (Var.Map.filter
        (fn k => k=KnownFun) funkinds)))))
  )
  
end

end