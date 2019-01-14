(*======================================================================*)
(* Translation of special constants  	                                *)
(*======================================================================*)
structure TransSCon =
struct

(*----------------------------------------------------------------------*)
(* Convert a typed constant into a runtime constant, returning          *)
(* NONE if there is an overflow.					*)
(*----------------------------------------------------------------------*)
fun trans (scon, ty) =
  case scon of

  (* Strings go straight through *)
    SCon.StrCon s => 
    SOME (Constants.STRING s)

  (* For characters we look up their code. Beware Unicode/ASCII! *)
  | SCon.CharCon c => 
    SOME (Constants.CHAR c)

  (* Reals are not supported by this function *)
  | SCon.RealCon s =>
    (case Real.fromString s of
      NONE => 
      NONE

    | SOME r => 
      let 
        val SOME tn = MILTy.fromTyname ty 
      in
        if TyName.eq(tn, TyNames.real64TyName)
        then SOME (Constants.DOUBLE (RTDouble.fromReal r))
        else SOME (Constants.FLOAT (RTFloat.fromReal r))
      end
    )


  (* For integers and words we check for overflow *)
  | SCon.NumCon(base,kind,s) =>
    let
      val SOME tn = MILTy.fromTyname ty
    in
      if TyName.eq(tn, TyNames.int8TyName)
      then 
      (case RTInt.fromString base kind s of
        NONE => NONE
      | SOME n => if RTInt.isji1 n then SOME (Constants.BYTE n) else NONE)
      else

      if TyName.eq(tn, TyNames.int16TyName)
      then
      (case RTInt.fromString base kind s of
        NONE => NONE
      | SOME n => if RTInt.isji2 n then SOME (Constants.SHORT n) else NONE)
      else

      if TyName.eq(tn, TyNames.int32TyName)
      then
      Option.map Constants.INT (RTInt.fromString base kind s)
      else

      if TyName.eq(tn, TyNames.int64TyName)
      then
      Option.map Constants.LONG (RTLong.fromString base kind s)
      else
    
      if TyName.eq(tn, TyNames.word8TyName)
      then 
      (case RTInt.fromString base kind s of
        NONE => NONE
      | SOME n => if RTInt.isju1 n then SOME (Constants.BYTE n) else NONE)
      else 

      if TyName.eq(tn, TyNames.word16TyName)
      then 
      (case RTInt.fromString base kind s of
        NONE => NONE
      | SOME n => if RTInt.isju2 n then SOME (Constants.SHORT n) else NONE)
      else 

      if TyName.eq(tn, TyNames.word32TyName)
      then
      Option.map Constants.INT (RTInt.fromString base kind s)
      else

      if TyName.eq(tn, TyNames.word64TyName)
      then
      Option.map Constants.LONG (RTLong.fromString base kind s)    

      else Debug.fail "TransSCon.trans"
    end


end