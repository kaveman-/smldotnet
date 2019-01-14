(*----------------------------------------------------------------------*)
(* Finite maps for runtime constants					*)
(*----------------------------------------------------------------------*)
structure CMap = MapFn(
  struct
    type ord_key = Constants.constant
    fun compare (c1,c2) =
    case (c1,c2) of
    (Constants.BYTE n1, Constants.BYTE n2) => RTInt.numops.compare(n1,n2)
  | (Constants.INT n1, Constants.INT n2) => RTInt.numops.compare(n1,n2)
  | (Constants.SHORT n1, Constants.SHORT n2) => RTInt.numops.compare(n1,n2)
  | (Constants.CHAR n1, Constants.CHAR n2) => RTInt.numops.compare(n1,n2)
  | (Constants.STRING s1, Constants.STRING s2) => UString.compare(s1,s2)
  | (Constants.LONG n1, Constants.LONG n2) => RTLong.numops.compare(n1,n2)
  | (Constants.FLOAT _, Constants.FLOAT _) => LESS
  | (Constants.DOUBLE _, Constants.DOUBLE _) => LESS (* Hack *)
  | _ => Debug.fail ("CMap.compare: " ^ 
    Constants.constant_toString c1 ^ " and " ^ Constants.constant_toString c2)
  end)

