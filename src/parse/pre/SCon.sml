(*======================================================================*)
(* Special constants							*)
(*======================================================================*)
structure SCon =
struct
   local 
      open IntConvFlags 
   in
      datatype SCon = 
        NumCon of Base*Kind*string
      | StrCon of UString.t
      | CharCon of RTInt.t
      | RealCon of string
      
      (*--------------------------------------------------------------------*)
      (* Convert a special constant into its original string form for	    *)
      (* pretty-printing.                                                   *)
      (*--------------------------------------------------------------------*)
      fun toString scon =
        (case scon of
          NumCon(Hex, Signed false, s) => "0x" ^ s
        | NumCon(Hex, Signed true, s) => "~0x" ^ s
        | NumCon(Decimal, Signed false, s) => s
        | NumCon(Decimal, Signed true, s) => "~" ^ s
        | NumCon(Hex, Unsigned, s) => "0wx" ^ s
        | NumCon(Decimal, Unsigned, s) => "0w" ^ s
        | RealCon s => s
        | StrCon s => "\"" ^ UString.toMLString s ^ "\""
        | CharCon c => 
          let
             val SOME jc=UChar.fromRTInt c
          in
             "#\"" ^ (UChar.toMLescape jc) ^ "\""
          end
        )
   end
end



