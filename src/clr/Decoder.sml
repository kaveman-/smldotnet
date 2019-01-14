(*======================================================================*)
(* Decode the intermediate format used to pass class info from .NET	*)
(*======================================================================*)
structure Decoder =
struct

fun decodeClass (longid:Longid.longid,filename) =
let
  val f = TextIO.openIn filename
  val linenumber = ref 0
  fun strip_nl s = String.substring(s,0,String.size(s) - 1)
  fun readLine () = 
  let val strOpt = TextIO.inputLine f
  in
    linenumber := !linenumber + 1;
    strOpt
  end

  fun warning s =
    PrintManager.println("[Warning: " ^ s ^ " for " ^ Longid.toString longid ^ " at line " ^ 
      Int.toString (!linenumber) ^ "]")

  fun error s =
    (PrintManager.println("[Error: " ^ s ^ " for " ^ Longid.toString longid ^ "]");
     TextIO.closeIn f; OS.FileSys.remove filename; NONE)

  fun decodeClass ch s =
  (let
    val (assembly, s) = Substring.splitl (fn c => c <> #"!") s
    val SOME (#"!", s) = Substring.getc s
    val (class, s) = Substring.splitl (fn c => c <> #"!") s
    val SOME (#"!", s) = Substring.getc s
    val depth = ref 0 (* class nesting depth *)
    val longid = map (Id.fromString o Substring.string) 
	             (Substring.tokens (fn c => 
					c = #"." orelse 
					(c = #"+" andalso 
					 (depth:= !depth+1;true)))
					class)
    val assembly = Id.fromString (Substring.string assembly)
  in
    (SMLTy.baseType (
    case ch of
      #"C" => TyName.externalEq (assembly, longid, !depth)
    | #"c" => TyName.external (assembly, longid, !depth)
    | #"V" => TyName.externalValEq (assembly, longid, !depth)
    | #"v" => TyName.externalVal (assembly, longid, !depth)
    ), s)
  end) handle e => 
  (PrintManager.println("Error: could not parse class name " ^ Substring.string s ^ " in " ^ 
   Longid.toString longid ^ " at line " ^ Int.toString(!linenumber));
   raise e)

  fun decodeType s =
    case Substring.getc s of
    SOME (#"a", s) => 
    let val (t, s) = decodeType s
    in
      (SMLPrimTy.optionType (SMLTy.arrayType t), s)
    end

  | SOME (#"&", s) => 
    let val (t, s) = decodeType s
    in
      (SMLTy.addressType t, s)
    end
(* SL: or *)
(*
  | SOME (ch as (#"C" | #"c"), s) =>
    let
      val (t, s) = decodeClass ch s
    in
      (SMLPrimTy.optionType t, s)
    end
  | SOME (ch as (#"V" | #"v"), s) => 
    decodeClass ch s
*)
  | SOME (ch as (#"C"), s) =>
    let
      val (t, s) = decodeClass ch s
    in
      (SMLPrimTy.optionType t, s)
    end
  | SOME (ch as (#"c"), s) =>
    let
      val (t, s) = decodeClass ch s
    in
      (SMLPrimTy.optionType t, s)
    end
  | SOME (ch as (#"V"), s) => decodeClass ch s
  | SOME (ch as (#"v"), s) => decodeClass ch s

  | SOME (_, s) =>
    decodeType s

  fun decodeClassHeader s = 
  let
    fun decode (flags, super : SMLTy.Type option, interfaces : SMLTy.Type list) s =
    case Substring.getc s of
(* SL: or *)
(*
      (NONE | SOME(#"\n", _)) => 
      (flags, super, interfaces)
*)
      NONE => (flags, super, interfaces)
    | SOME(#"\n", _) => (flags, super, interfaces)

    | SOME (c, s) =>
      case c of
	#"s" => decode (Symbol.Set.add(flags, Id.sealedSym), super, interfaces) s
      | #"a" => decode (Symbol.Set.add(flags, Id.abstractSym), super, interfaces) s
      | #"p" => decode (Symbol.Set.add(flags, Id.publicSym), super, interfaces) s
      | #"v" => decode (flags, super, interfaces) s
      | #"i" => decode (Symbol.Set.add(flags, Id.interfaceSym), super, interfaces) s
      | #":" => 
        (case Substring.getc s of
          SOME (ch,s) =>
	  let
	    val (super, s) = decodeClass ch s
  	  in
	    decode (flags, SOME super, interfaces) s
	  end)
      | #"$" =>
        (case Substring.getc s of
          SOME (ch,s) =>
	  let
	    val (c, s) = decodeClass ch s
  	  in
	    decode (flags, super, c::interfaces) s
	  end)
      | _ =>
	decode (flags, super, interfaces) s
  in
    decode (Symbol.Set.empty, NONE, []) s
  end

  fun decodeField s = 
  let
    fun decode (flags, name, ty, value) s =
    case Substring.getc s of
(* SL: or *)
(*
      (NONE | SOME (#"\n", _)) => 
      { name = Id.fromString name, ty = ty, value = value, flags = flags }
*)
      NONE => 
      { name = Id.fromString name, ty = ty, value = value, flags = flags }
    | SOME (#"\n", _) => 
      { name = Id.fromString name, ty = ty, value = value, flags = flags }

    | SOME (c, s) =>
      case c of
	#"f" => decode (Symbol.Set.add(flags, Id.finalSym), name, ty, value) s
      | #"p" => decode (Symbol.Set.add(flags, Id.publicSym), name, ty, value) s
      | #"t" => decode (Symbol.Set.add(flags, Id.transientSym), name, ty, value) s
      | #"P" => decode (Symbol.Set.add(flags, Id.protectedSym), name, ty, value) s
      | #"s" => decode (Symbol.Set.add(flags, Id.staticSym), name, ty, value) s
      | #":" => 
	let
	  val (ty, s) = decodeType s
	in
	   decode (flags, name, ty, value) s
	end
      | #"\"" =>
	let
	  val (name, s) = Substring.splitl (fn c => c <> #"\"") s
	  val SOME (#"\"", s) = Substring.getc s
	in
	  decode (flags, Substring.string name, ty, value) s
	end            
       | #"=" =>
	  let val (ty,s) = decodeType s
	      val (value,s) = decodeLiteral ty s
	  in	
	      decode (flags,name,ty,value) s
	  end	
       | _ => decode (flags, name, ty, value) s
    and decodeSign s = case Substring.getc s of 
		       SOME(#"-",s') => (true,s') 
		     | SOME(_,_) => (false,s)
    and decodeLiteral ty s = 
    if SMLTy.eq(ty, SMLPrimTy.optionType SMLPrimTy.stringType)
    then
      let
	val (strOpt, s) = Substring.splitl (fn #"\n" => false | _ => true) s
      in
       ((if Substring.isPrefix "null" strOpt then SOME (Constants.NULL)
       else let val ints = Substring.tokens (fn #" " => true | _ => false) strOpt
	    in  SOME (Constants.STRING (
		       UString.fromUnicode (
			    map (valOf o
				 UChar.fromRTInt o 
				 valOf o 
				 (RTInt.fromString IntConvFlags.Decimal IntConvFlags.Unsigned)
				 o Substring.string)
				 ints)))
	    end)
       handle _ => 
	   (warning ("could not parse constant String " ^ Substring.string strOpt); NONE),
      s)
      end
    else
    let
      val SOME ([],tyname) = SMLTy.fromConsType ty
    in
      if TyName.eq(tyname, TyNames.boolTyName)
      then
        let
	  val (digits, s) = Substring.splitl (fn c => Char.isAlpha c orelse Char.isDigit c) s
	  val someTrue = SOME (Constants.BOOLEAN (RTInt.fromInt32 1))  (*@TODO: not sure about this *)
	  val someFalse = SOME (Constants.BOOLEAN (RTInt.fromInt32 0)) (*@TODO: not sure about this *)
	  val value = 
	    case (Substring.string digits) of
	      "True" => someTrue
	    | "False" => someFalse
	    | "1" => someTrue
	    | "0" => someFalse
	    | _ =>
	    (warning ("could not parse constant Boolean " ^ Substring.string digits); NONE)
	in	
	    (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.int8TyName) 
      then
	let
	  val (sign,s) = decodeSign s
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTInt.fromString IntConvFlags.Decimal (IntConvFlags.Signed sign) (Substring.string digits) of
	      NONE => (warning("could not parse constant Byte "	^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.BYTE n)
  	in	
	    (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.charTyName)
      then
        let
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTInt.fromString IntConvFlags.Decimal IntConvFlags.Unsigned (Substring.string digits) of
	      NONE => (warning("could not parse character constant " ^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.CHAR n)
	in	
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.real64TyName)
      then
	let
	  val (chars, s) = 
	    Substring.splitl (fn c => c = #"-" orelse c = #"+" orelse 
					  Char.isDigit c  orelse Char.isAlpha c orelse
					  c = #"." orelse c = #"e" orelse c = #"E") s
	  val value =
	  case Substring.string chars of
	     "NaN" =>  SOME (Constants.DOUBLE (RTDouble.fromReal (Real.posInf + Real.negInf)))
	  |  "Infinity" => SOME (Constants.DOUBLE (RTDouble.fromReal (Real.posInf)))
	  |  "-Infinity" => SOME (Constants.DOUBLE (RTDouble.fromReal (Real.negInf)))
	  | s' => (case (Real.fromString s') of
		    NONE => (warning("could not parse Double constant "^s');NONE)
		  | SOME r => SOME (Constants.DOUBLE (RTDouble.fromReal r)))
	in		       
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.real32TyName)
      then	  
	let
	  val (chars, s) = 
	    Substring.splitl (fn c => c = #"-" orelse c = #"+" orelse 
					  Char.isDigit c orelse Char.isAlpha c orelse
					  c = #"." orelse c = #"e" orelse c = #"E") s
	  val value =
	  case Substring.string chars of
	     "NaN" =>  SOME (Constants.FLOAT (RTFloat.fromReal (Real.posInf + Real.negInf)))
	  |  "Infinity" => SOME (Constants.FLOAT(RTFloat.fromReal (Real.posInf)))
	  |  "-Infinity" => SOME (Constants.FLOAT (RTFloat.fromReal (Real.negInf)))
	  | s' => (case (Real.fromString s') of
		    NONE => (warning("could not parse Float constant "^s');NONE)
		  | SOME r => SOME (Constants.FLOAT (RTFloat.fromReal r)))
	in		       
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.int32TyName)
      then
	let
	  val (sign,s) = decodeSign s
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTInt.fromString IntConvFlags.Decimal (IntConvFlags.Signed sign) (Substring.string digits) of
	      NONE => (warning("could not parse constant Int " ^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.INT n)
	in	
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.int64TyName)
      then
	let
	  val (sign,s) = decodeSign s
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTLong.fromString IntConvFlags.Decimal (IntConvFlags.Signed sign) (Substring.string digits) of
	      NONE => (warning("could not parse constant Long "	^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.LONG n)
	in	
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.int16TyName)
      then
	let
	  val (sign,s) = decodeSign s
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTInt.fromString IntConvFlags.Decimal (IntConvFlags.Signed sign) (Substring.string digits) of
	      NONE => (warning("could not parse constant Int " ^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.SHORT n)
	in	
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.word32TyName)
      then
	let
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTInt.fromString IntConvFlags.Decimal IntConvFlags.Unsigned (Substring.string digits) of
	      NONE => (warning("could not parse constant Unsigned " ^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.INT n)
	in	
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.word8TyName)
      then
	let
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTInt.fromString IntConvFlags.Decimal IntConvFlags.Unsigned (Substring.string digits) of
	      NONE => (warning("could not parse constant Unsigned Byte" ^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.BYTE n)
	in	
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.word16TyName)
      then
	let
	  val (digits, s) = Substring.splitl Char.isDigit s
	  val value = 
	    case RTInt.fromString IntConvFlags.Decimal IntConvFlags.Unsigned (Substring.string digits) of
	      NONE => (warning("could not parse constant Unsigned Byte"	^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.INT n)
	in	
	   (value,s)
	end
      else
      if TyName.eq(tyname, TyNames.word64TyName)
      then
	let
	  val (digits, s) = 
	    Substring.splitl Char.isDigit s
	  val value = 
	    case RTLong.fromString IntConvFlags.Decimal IntConvFlags.Unsigned (Substring.string digits) of
	      NONE => (warning("could not parse constant Unsigned Byte"	^ Substring.string digits); NONE)
	    | SOME n => SOME (Constants.LONG n)
	in	
	   (value,s)
	end
      else
      (* Must be an enum *)
      let
         val (digits, s) = Substring.splitl Char.isDigit s
         val value = 
	   case RTInt.fromString IntConvFlags.Decimal IntConvFlags.Unsigned (Substring.string digits) of
		 NONE => (warning("could not parse constant Value " ^ Substring.string digits); NONE)
	       | SOME n => SOME (Constants.INT n)
      in	
	(value,s)
      end	
    end (* let *)
  in
    decode (Symbol.Set.empty, "", SMLTy.baseType TyNames.int32TyName, NONE) s
  end (* decodeField *);


  fun decodeArgTys s =
  let
    val SOME (#"(", s) = Substring.getc s

    fun decode tys s =
    case Substring.getc s of
      SOME (#")", s) => (rev tys, s)
    | SOME (#",", s) => decode tys s
    | _ =>
      let
	val (t,s) = decodeType s
      in 
	decode (t::tys) s
      end
  in
    decode [] s
  end

  fun decodeMethodType s =
  let
    val (result, s) = 
    case Substring.getc s of
      SOME (#"%", s) => (NONE, s)
    | _ => 
      let val (t,s) = decodeType s
      in
	(SOME t, s)
      end

    val (args,s) = decodeArgTys s
  in
    (args, result, s)
  end

  fun decodeMethod s = 
  let
    fun decode (flags, name, argtys, resty) s =
    case Substring.getc s of
(* SL: or *)
(*
      (NONE | SOME (#"\n", _)) => 
      { name = Id.fromString name, argtys = argtys, resty = resty, flags = flags }
*)
      NONE =>
      { name = Id.fromString name, argtys = argtys, resty = resty, flags = flags }
    | SOME (#"\n", _) => 
      { name = Id.fromString name, argtys = argtys, resty = resty, flags = flags }

    | SOME (c, s) =>
      case c of
	#"f" => decode (Symbol.Set.add(flags, Id.finalSym), name, argtys, resty) s
      | #"p" => decode (Symbol.Set.add(flags, Id.publicSym), name, argtys, resty) s
      | #"P" => decode (Symbol.Set.add(flags, Id.protectedSym), name, argtys, resty) s
      | #"s" => decode (Symbol.Set.add(flags, Id.staticSym), name, argtys, resty) s
  (*
      | #"n" => decode (Method.NATIVE::flags, name, argtys, resty) s
  *)
      | #"a" => decode (Symbol.Set.add(flags, Id.abstractSym), name, argtys, resty) s
  (*
      | #"y" => decode (Method.SYNCHRONIZED::flags, name, argtys, resty) s
  *)
      | #"v" => decode (flags, name, argtys, resty) s
      | #":" => 
	let
	  val (argtys, resty, s) = decodeMethodType s
	in
	  decode (flags, name, argtys, resty) s
	end

      | #"\"" =>
	let
	  val (name, s) = Substring.splitl (fn c => c <> #"\"") s
	  val SOME (#"\"", s) = Substring.getc s
	in
	  decode (flags, Substring.string name, argtys, resty) s
	end            

      | _ =>
	decode (flags, name, argtys, resty) s
  in
    decode (Symbol.Set.empty, "", [], NONE) s
  end

  fun decodeConstructor s = 
  let
    fun decode (flags, tys) s =
    case Substring.getc s of
(* SL: or *)
(*
      (NONE | SOME (#"\n", _)) => 
      { name = Id.fromString "<init>", resty = NONE, argtys = tys, flags = flags }
*)
      NONE => 
      { name = Id.fromString "<init>", resty = NONE, argtys = tys, flags = flags }
    | SOME (#"\n", _) => 
      { name = Id.fromString "<init>", resty = NONE, argtys = tys, flags = flags }

    | SOME (c, s) =>
      case c of
	#"p" => decode (Symbol.Set.add(flags, Id.publicSym), tys) s
      | #"P" => decode (Symbol.Set.add(flags, Id.protectedSym), tys) s
      | #":" => 
	let
	  val (tys, s) = decodeArgTys s
	in
	  decode (flags, tys) s
	end

  in
    decode (Symbol.Set.empty, []) s
  end
in
  (if strip_nl(valOf(readLine())) <> "getmeta v12" 
  then (error "wrong version of cached metadata")
  else
  let
    val assemblyFile = strip_nl(valOf(readLine ()))
    val stamp = strip_nl(valOf(readLine ()))
    val s = valOf(readLine ())
    val (flags, super, interfaces) = decodeClassHeader (Substring.all s)
    fun readFields fields =
    case readLine () of
      NONE => fields
    | SOME s => if String.sub(s,0) = #"!"
                then fields
                else readFields (decodeField (Substring.all s)::fields)

    fun readMethods methods =
    case readLine () of
      NONE => methods
    | SOME s => if String.sub(s,0) = #"!"
                then methods
                else readMethods (decodeMethod (Substring.all s)::methods)

    fun readConstructors methods =
    case readLine () of
      NONE => methods
    | SOME s => if String.sub(s,0) = #"!"
                then methods
                else readConstructors (decodeConstructor (Substring.all s)::methods)

    val fields = readFields []
    val methods = readMethods [] @ readConstructors []
  in
    (TextIO.closeIn f;
     SOME (
	   {assemblyFile=assemblyFile,
	    stamp=stamp,
	    info=
	    { 
	     longid = longid,
	     super = super,
	     flags = flags,
	     interfaces = interfaces,
	     fields = fields,
	     methods = methods
	     }}))
  end)
  handle e => error (exnMessage e)
end


end

