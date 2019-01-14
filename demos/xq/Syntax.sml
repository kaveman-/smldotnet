structure Syntax =
struct

datatype ScalarConst = I of int | B of bool | S of string
type tag = string
datatype XMLData = C of ScalarConst | Elem of tag * (XMLData list)
type XMLForest = XMLData list

(* Abstract syntax for Wadler et al's language *)
datatype XMLExp = EConst of ScalarConst
                | EVar of string
                | EElem of tag * XMLExp
                | EEmptySeq
                | EConcat of XMLExp * XMLExp
                | ECond of XMLExp * XMLExp * XMLExp
                | ELet of string * XMLExp * XMLExp
                | EFor of string * XMLExp * XMLExp
                | EValue of XMLExp
                | EProject of tag * XMLExp
                | EChildren of XMLExp
                | EFunApp of string*(XMLExp list)
                | EFunDef of string*(string list)*XMLExp*XMLExp

exception Error of string
type XFun = XMLForest list -> XMLForest
fun PI [(C (I n))] = n
fun EI n = [C (I n)]
val int = (EI,PI)
fun PB [(C (B b))] = b
fun EB b = [C (B b)]
val bool = (EB,PB)
fun PS [(C (S s))] = s
fun ES s = [C (S s)]
val string = (ES,PS)
infix **
fun (e,p) ** (e',p') = (fn (x,y)=>(e x) @ (e' y),fn [x,y]=>(p x, p' y))
infix -->
fun (e,p) --> (e',p') = (fn f =>  e' o f o p, fn f => p' o f o e)
fun embed (e,p) = e
fun project (e,p) = p

type FEnv = string -> XFun
type VEnv = string -> XMLForest
fun extend env (x,v) = fn y => if y=x then v else env y
fun bigextend env ps = case ps of
 ([],[]) => env
| (x::xs,v::vs) => bigextend (extend env (x,v)) (xs,vs)
| _ => raise Fail "length mismatch in bigextend"

val emptyvenv = fn x=> raise Error "unbound variable"
val emptyfenv = fn x=> raise Error "unbound function name"
val emptyenv = (emptyfenv,emptyvenv)

fun preenv t l = map (fn (n,v) => (n,embed t v)) l

val ariths = preenv ((int**int)-->int)
             [("+", op +),
              ("*", op * ),
              ("-", op -)]
val comps = preenv ((int**int)-->bool)
             [(">", op >),
              (">=", op >=),
              ("<", op <),
              ("<=", op <=)]


val embequal = fn ([x,y:XMLData list]) => [C(B(x=y))]

val embnotequal = fn ([x,y:XMLData list]) => [C(B(x<>y))]

val embsum = fn [l] => [C(I(foldl (fn (C(I(n)),m)=>n+m) 0 l))]

val embempty = fn [l] => EB (l=[])

fun distinct ([],ys) = rev ys
  | distinct (x::xs,ys) = if List.exists (fn z => x=z) ys then distinct (xs,ys)
                          else distinct (xs,x::ys)
val embdistinct = fn [l] => distinct (l,[])

val miscs = [("=",embequal), ("sum",embsum),("<>",embnotequal),
("empty",embempty),
  ("distinct",embdistinct) 
]

val basefenv = bigextend emptyfenv (ListPair.unzip (ariths @ comps @ miscs))

val baseenv = (basefenv,emptyvenv)

fun children (Elem(_,kids)) = kids
fun project s = List.filter (fn (Elem(t,_))=>s=t | _ => false)
val dataof = List.filter (fn (C s) => true | _ => false)

fun unit x = [x]
infix >>=
fun e >>= f = List.concat (List.map f e)

(* this version requires a forest on the left *)
infix //
fun e // t = e >>= (fn b => project t (children b))

fun data e = e >>= (fn b => dataof (children b))

fun interpret (env as (fenv,venv)) exp = case exp of
   EConst c => [C c]
 | EVar s => venv s
 | EFunApp (fname,arglist) => let val f = fenv fname
                                  val args = map (interpret env) arglist
                              in f args
                              end
 | EFunDef (fname,argnames,fbody,inwhat) =>
     let fun f args = interpret (fenv,bigextend venv (argnames, args)) fbody
     in interpret (extend fenv (fname, f), venv) inwhat
     end
 | EElem (t,e) => [Elem(t,interpret env e)]
 | EEmptySeq => []
 | EConcat (e1,e2) => (interpret env e1) @ (interpret env e2)
 | ECond (b,e1,e2) => (case interpret env b of 
                          [C(B(true))] => interpret env e1
                        | [C(B(false))] => interpret env e2
                        | _ => raise Error "non [bool] in if"
                      )
 | ELet (v,e1,e2) => let val venv' = extend venv (v, interpret env e1)
                    in interpret (fenv,venv') e2
                    end
 | EFor (v,e1,e2) => let val rs = interpret env e1
                    in List.concat 
                        (List.map (fn x => interpret (fenv,extend venv (v,[x])) e2) rs)
                    end
 | EValue e => data (interpret env e)
 | EProject(t,e) =>  (interpret env e) // t
 | EChildren e => (let val [Elem(_,children)] = interpret env e
                   in children
                   end handle Match => raise Error "children() horribly wrong"
                  )

fun pc (I n) = Int.toString n
  | pc (B b) = if b then "true" else "false"
  | pc (S s) = "\"" ^ s ^ "\""

fun pd (C c) = pc c
  | pd (Elem(t,dl)) = t ^ "[" ^ (pdl dl) ^ "]"
and pdl [] = ""
  | pdl [d] = pd d
  | pdl (d::ds) = (pd d) ^ "," ^ (pdl ds)

end