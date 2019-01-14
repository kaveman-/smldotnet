structure ElabSCon =
struct

local open SMLTy ElabState
in

(*----------------------------------------------------------------------*)
(* Determine the type of a special constant				*)
(*----------------------------------------------------------------------*)
fun typeSCon (SCon.NumCon(_, IntConvFlags.Signed _, _)) = 
    tyVarType (freshTyVar 
      (TyVar.Overloaded (TyName.Set.addList(TyName.Set.empty,
        [TyNames.int16TyName, TyNames.int8TyName, 
         TyNames.int32TyName, TyNames.int64TyName]))))

  | typeSCon (SCon.NumCon(_, IntConvFlags.Unsigned, _)) = 
    tyVarType (freshTyVar 
      (TyVar.Overloaded (TyName.Set.addList(TyName.Set.empty,
        [TyNames.word32TyName, TyNames.word8TyName, 
         TyNames.word16TyName, TyNames.word64TyName]))))

  | typeSCon (SCon.StrCon _)  = 
    SMLPrimTy.stringType

  | typeSCon (SCon.RealCon _) = 
    tyVarType (freshTyVar 
      (TyVar.Overloaded (TyName.Set.addList(TyName.Set.empty,
        [TyNames.real64TyName, TyNames.real32TyName]))))

  | typeSCon (SCon.CharCon _) = 
    SMLPrimTy.charType

end

end