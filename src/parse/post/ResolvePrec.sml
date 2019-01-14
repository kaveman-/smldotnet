(*======================================================================*)
(* Resolve infix for expressions or patterns				*)
(*======================================================================*)
structure ResolvePrec :> sig
  exception InfixError of string
  val parse :  
  {
    apply: 'a * 'a -> 'a, 
    pair: 'a * 'a -> 'a,
    asId : 'a -> Id.id option
  } -> 
  Fixity.Env * 'a list -> 
  'a
end 
=
struct    

exception InfixError of string

open Fixity

datatype 'a precStack 
  = INf of Id.id * int * 'a * 'a precStack
  | NONf of 'a * 'a precStack
  | NILf

fun parse {apply,pair,asId} =
  let fun ensureNONf((e,Nonfix,_),p) = NONf(e,p)
        | ensureNONf((e,Infix _,SOME sym),p) = 
	   raise InfixError
	      ("expression or pattern begins with infix identifier \"" 
	       ^ Id.toString sym ^ "\"")
	| ensureNONf _ = Debug.fail "precedence:ensureNONf"

      fun start token = ensureNONf(token,NILf)

      fun trans (n,b) = if b then (n+n+1,n+n) else (n+n,n+n+1)

      (* parse an expression *)
      fun parse(NONf(e,r), (e',Nonfix,_)) = NONf(apply(e,e'),r)
        | parse(p as INf _, token) = ensureNONf(token,p)
        | parse(p as NONf(e1,INf(_,bp,e2,NONf(e3,r))), 
                (e4, f as Infix infixprec,SOME sym))=
            let val (lbp,rbp) = trans infixprec
            in
	    if lbp > bp then INf(sym,rbp,e4,p)
            else (if lbp = bp
		  then raise InfixError "mixed left- and right-associative operators of same precedence"
		  else ();
	          parse(NONf(apply(e2,pair (e3,e1)),r),(e4,f,SOME sym)))
            end

        | parse(p as NONf _, (e',Infix infixprec,SOME sym)) = 
          let val (lpb,rbp) = trans infixprec
          in
            INf(sym,rbp,e',p)
          end
        | parse _ = Debug.fail "Precedence.parse"
     
      (* clean up the stack *)
      fun finish (NONf(e1,INf(_,_,e2,NONf(e3,r)))) = 
		     finish(NONf(apply(e2,pair (e3,e1)),r))
        | finish (NONf(e1,NILf)) = e1
        | finish (INf(sym,_,e1,NONf(e2,p))) =
		     (raise InfixError
		      ("expression or pattern ends with infix identifier \"" 
		       ^ Id.toString sym ^ "\""))
        | finish NILf = Debug.fail "Corelang.finish NILf"
        | finish _ = Debug.fail "Corelang.finish"

   in fn (env, items as item1 :: items') =>
        let fun getfix item =
        let val fixity = asId item
        in
          (item, case fixity of NONE => Nonfix
                              | SOME sym => lookup(env,sym),
               fixity)
        end

            fun loop(state, a::rest) = loop(parse(state,getfix a),rest)
              | loop(state,nil) = finish state

         in loop(start(getfix item1), items')
        end
       | _ => Debug.fail "precedence:parse"
  end

end

