(*======================================================================*)
(* Pretty-print MIL terms						*)
(*======================================================================*)
structure MILPretty :> MILPRETTY =
struct

(* Display types at all? *)
val showTypes = Controls.add false "dmp.showTypes"

(* Display attributes? *)
val showAttributes = Controls.add true "dmp.showAttributes"

(* Display census info on bound variables? *)
val showOccs = Controls.add false "dmp.showOccs"

infixr 6 ++		(* Concatenation *)
infixr 6 +/		(* Concatenation with space *or* newline *)
infixr 5 //		(* Concatenation with newline *)

open MILTerm NewPretty

(* Set of term-bound type variables and ordinary variables *) 
val boundTyVars = ref (Var.Set.empty)
val boundVars = ref (Var.Set.empty)
val errors = ref false

(*----------------------------------------------------------------------*)
(* Can a value expression be displayed without parentheses?		*)
(*----------------------------------------------------------------------*)
fun isAtomVal (Var _) = true
  | isAtomVal (SCon _) = true
  | isAtomVal (Inj (_,_,[],_)) = true
  | isAtomVal (ExCon(_,[])) = true
  | isAtomVal (Tuple []) = true
  | isAtomVal (Unfold _) = true
  | isAtomVal _ = false

(* Keywords are displayed in bold in HTML mode *)
fun keyword x = bold (text x)

(* Variables are displayed in italics in HTML mode *)
fun variable x = italic (text x)

(* Bad variables are displayed in red in HTML mode *)
fun badvariable x = coloured "RED" (text x)

(*----------------------------------------------------------------------*)
(* Print a sequence							*)
(*----------------------------------------------------------------------*)
fun seq (empty,left1,right1,left2,right2,sep) f [] = text empty
  | seq (empty,left1,right1,left2,right2,sep) f [x] = text left1 ++ f x ++ text right1
  | seq (empty,left1,right1,left2,right2,sep) f (x::xs) =
    let
      fun loop [] = text right2
        | loop (x::xs) = text sep ++ f x ++ loop xs
    in
      text left2 ++ f x ++ loop xs
    end

(*----------------------------------------------------------------------*)
(* Prefix used to qualify function definitions				*)
(*----------------------------------------------------------------------*)
fun kindToString AnyFun = ""
  | kindToString LocalFun = "block "
  | kindToString KnownFun = "known "

fun pType ty = coloured "GREEN" (MILTy.pTy ty) 
fun pCmpType cty = coloured "GREEN" (MILTy.pCmpTy cty)
fun pId id = coloured "BLUE" (text (Id.toString id))
fun pLongid id = coloured "BLUE" (text (Longid.toString id))

(*----------------------------------------------------------------------*)
(* Stringify a bound variable and insert it into the vars map		*)
(*----------------------------------------------------------------------*)
fun pBoundVar ((x,longid) : MILTerm.BoundVar) =
let 
  val s = Var.toString x
  val internal = (* if not (Var.isDummy x) andalso Var.Set.member(!boundVars, x)
                 then badvariable s
                 else (boundVars := Var.Set.add(!boundVars, x);  *) (xdef s (variable s))

  val withsym = if List.null longid
	        then internal
                else internal ++ text "{" ++ pLongid longid ++ text "}"
in
  if Controls.get showOccs
  then withsym ++ text ("[" ^ Int.toString (Census.getVar x) ^ "]")
  else withsym
end
	     
(*----------------------------------------------------------------------*)
(* Do the same for a typed bound variable				*)
(*----------------------------------------------------------------------*)
fun pTypedVar (x,ty) =
  if Controls.get showTypes
  then pBoundVar x ++ text ":" ++ pType ty
  else pBoundVar x
  
val pTypedVars = seq ("<>", "", "", "<", ">", ",") pTypedVar
val pBoundVars = seq ("<>", "", "", "<", ">", ",") pBoundVar
val pTyArgs = seq (" {}", " {", "}", " {", "}", ",") pType
val pTyVarBounds = seq ("", "Fn ", "=> ", "Fn ", "=> ", ",") (MILTy.pBoundTyVar)

fun pArgs x  = seq ("<>", " ", "", "<", ">", ",") pVal x
and pTuple x = seq ("()", "(", ")", "(", ")", ",") pVal x

and pTAbstr (xs, body) = group (pTypedVars xs ++ text " => " +/ pCmp body)

and pVal v =
  case v of
    SCon (ty, c as Constants.STRING _) =>
    text "\"" ++ text (Constants.constant_toString c) ++ text "\""

  | SCon (ty, c) =>
    text (Constants.constant_toString c) ++
      (if Controls.get showTypes then text ":" ++ pType ty else empty)

  | Var x => 
    let val s = Var.toString x
    in
      xref s (variable s)
    end

  | Inj(ty, i, vs, si) => 
    let
      val typestr = 
        if Controls.get showTypes  
        then text "{" ++ pType ty ++ text "}"
        else empty
      val sistr = text "{" ++ pId si ++ text "}"
    in
      text ("in_" ^ Int.toString i) ++ typestr ++ sistr ++ pArgs vs
    end

  | TApp(v, tys) =>	    	pVal v ++ pTyArgs tys
  | TAbs(tyvars, v) =>	    	pTyVarBounds tyvars ++ pVal v
  | ExCon(ty, vs) =>	    	pType ty ++ pArgs vs
  | Tuple vs =>			pTuple vs
  | Proj(i, n, v) => 		text ("#" ^ Int.toString i) ++ text " " ++ pVal v
  | As(v, ty) =>		pVal v ++ text ":>" ++ pType ty 
  | Unfold v =>			keyword "unfold" ++ text "(" ++ pVal v ++ text ")"
  | Fold (v, ty) =>		
    let
      val typestr = 
        if Controls.get showTypes 
        then text "{" ++ pType ty ++ text "}"
        else empty
    in
      keyword "fold" ++ typestr ++ text " " ++ pVal v
    end

and pFunDef (kind,def) = 
  case def of
    Fun(f, tabs) => 
    keyword (kindToString kind) ++ pBoundVar f ++ text " = " ++ keyword "fn" ++ text " " ++ pTAbstr tabs

  | RecFun defs =>
    let
      val firstf = #1 (#1 (hd defs))
      val prefixes = map (fn (f,g,_,_) =>
            keyword(if Var.eq(#1 f, firstf) then kindToString kind else "and ") ++ pBoundVar f ++ 
            text " as " ++ pBoundVar g ++ keyword " = fn ") defs
    in
      stack (ListPair.map (fn (prefix,(_,_,tabs,cty)) =>
        prefix ++ pTAbstr tabs) (prefixes,defs))
    end

and pAttrs attributes = empty (*
    let val nl = newline indent
	fun dispAttribute (defargtys,defmilclass,bytes) =
	    (p (MILTy.toString (MILTy.inj(MILTy.Arrow(defargtys,MILTy.noeffect [defmilclass]))));
	     case Word8Vector.length(bytes) of 
 	       0 => ()
	     | _ => (p "["; Word8Vector.app (fn w8 => (p (Word8.toString w8); p " ")) bytes;p "]"))
    in
       if Controls.get showAttributes 
	   then case attributes of 
	          [] => ()
	        | _ => (  p "@{" 
			; List.app (fn att => (dispAttribute att;p " ")) attributes
		        ; p "}")
       else ()
    end *)
	 
and pClass (classname, (attributes,flags,super,implements), fields, methods) =
    let
      fun pField (name, mods, ty, eopt) =
           pId name ++ (if Controls.get showTypes then text " : " ++ pType ty else empty) ++ 
           (case eopt of NONE => empty | SOME e => text (" = " ^ Constants.constant_toString e))

      fun pMethod (name,attributes, mods, tys, tyopt, absopt) =
          let
            val argtys = 
              if Symbol.Set.member(mods, Id.staticSym)
              then tys
              else classname::tys
          in
            pId name ++
	    pAttrs attributes ++ 
            (case absopt of 
              NONE => if Controls.get showTypes then text ":" ++ 
		       (case tyopt of SOME ty => pType ty | _ => empty) else empty
            | SOME ((f,_),(xs,e)) => 
              (text (" as " ^ Var.toString f ^ " = ") ++ line ++ keyword "fn " ++ 
              pTAbstr (ListPair.zip(xs,argtys),e)))
          end
    in                 
      keyword "class" ++ text " " ++ pType classname ++
      (case super of NONE => empty | SOME ty => text " : " ++ pType ty) ++
      text " " ++ keyword "with" ++
      line ++ 
      nest 4 (pAttrs attributes ++ 
      stack (map pField fields) ++ 
      stack (map pMethod methods)) ++ line ++ text "end"
    end

(*----------------------------------------------------------------------*)
(* Pretty-print a computation expression				*)
(*----------------------------------------------------------------------*)
and pCmp e = 
let
  fun pCont (acc, e) =  
    case e of
      Let(e1,(xs,e2)) 		=> pCont ((pTypedVars xs ++ text " <= " ++ pCmp e1)::acc, e2)
    | LetVal(x,v,e2)   		=> pCont ((pBoundVar x ++ text " = " ++ pVal v) :: acc, e2)
    | LetFun(tvs,k,d,e2) 	=> pCont ((pTyVarBounds tvs ++ pFunDef (k,d)) :: acc, e2)
    | TryLet(e1,H,(xs,e2))	=> pCont ((pTypedVars xs ++ text " <= " ++ pCmp e1 ++
                             	   text " catch " ++ stack (map pTAbstr H))::acc, e2)
    | LetClass(c,i,f,m,e2) 	=> pCont (pClass (c,i,f,m)::acc, e2)
    | _ 			=> keyword "let" ++ text " " ++ nest 4 (stack (rev acc)) ++ line ++ group (keyword "in" ++ nest 4 (line ++ pCmp e))


  fun pCases (s, f, (v, cases, eopt, cty)) = 
     keyword s ++ text " " ++  pVal v ++ text " " ++ keyword "of" ++ (if Controls.get showTypes then text ":" ++ pCmpType cty else empty) ++
     nest 4 (line ++ stack (map (fn (i,(xs,e)) => group (f i ++ text " " ++ pBoundVars xs ++ text " => " +/ pCmp e)) cases @ 
                     (case eopt of NONE => [] | SOME e => [group (text "_ => " +/ pCmp e)])))
in
  case e of
    Special((Ext.Invoke, SOME ty, SOME meth), vs, cty) =>
    pType ty ++ text "." ++ pId meth ++ pArgs vs

  | Special((Ext.Invoke, NONE, SOME meth), v::vs, cty) =>
    pVal v ++ text ".#" ++ pId meth ++ pArgs vs

  | Special((Ext.GetField, SOME ty, SOME fld), _, cty) =>
    pType ty ++ text "." ++ pId fld

  | Special(j as (optype, tyopt, idopt), vs, cty) => 
    text (ExtOps.toString optype) ++ 
    (case tyopt of NONE => empty | SOME ty => text " " ++ pType ty) ++ 
    (case idopt of NONE => empty | SOME id => text " " ++ pId id) ++ 
    pArgs vs ++ 
    (text ":" ++ pCmpType cty)

    (* Let-bindings with a continuation are blocked together *)
  | Let _ => 		pCont ([], e)
  | LetVal _ => 	pCont ([], e)
  | LetFun _ => 	pCont ([], e)
  | LetClass _ =>       pCont ([], e)
  | TryLet _ =>         pCont ([], e)

    (* Case constructs *)
  | Case cases =>	pCases ("case", fn i => text "in_" ++ text (Int.toString i), cases)
  | CaseSCon cases =>	pCases ("case", fn c => text "CON" (* o Constants.constant_toString *), cases)
  | TypeCase cases =>	pCases ("typecase", pType, cases)

    (* Others *)
  | Throw(v, cty, m) => keyword "raise " ++ pVal v ++ text("{" ^ m ^ "}") ++ (if Controls.get showTypes then text ":" ++ pCmpType cty else empty)
  | App(v, vs) =>	pVal v ++ pArgs vs
  | Triv vs => 		keyword "val" ++ pArgs vs
  | Encap e =>		keyword "pure" ++ text "(" ++ pCmp e ++ text ")"
end

fun pretty pr d = 
(
  boundVars := Var.Set.empty;
  if Debug.html () then NewPretty.prettyHtml 120 pr d else NewPretty.pretty 80 pr d
)

fun dumpVal v = pretty Debug.print (pVal v)
fun dumpCmp e = 
(
  let val d = pCmp e
  in
    pretty Debug.print d
  end
)

fun dumpCmpTo filename e = 
let
  val f = TextIO.openOut filename
in
  pretty (fn s => TextIO.output(f, s)) (pCmp e);
  TextIO.closeOut f
end

(*----------------------------------------------------------------------*)
(* Failure error messages dumped to the log.                            *)
(*----------------------------------------------------------------------*)
fun failVal v message = 
  (Debug.print ("Fail: " ^ message ^ " in:\n"); dumpVal v;
  raise Fail message)

fun failCmp e message = 
  (Debug.print ("Fail: " ^ message ^ " in:\n"); dumpCmp e;
  raise Fail message)

fun errorVal v message = 
  (Debug.print (message ^ " in:\n"); dumpVal v; ())

fun errorCmp e message = 
  (Debug.print (message ^ " in:\n"); dumpCmp e; ())

end