(*======================================================================*)
(* Pattern match compilation; term descriptions				*)
(*======================================================================*)
structure PatDescr =
struct

(*----------------------------------------------------------------------*)
(* Term description							*)
(*----------------------------------------------------------------------*)
datatype termd =
    Pos of PatCon.con * termd list         (* All arguments in proper order *)
  | Neg of PatCon.con list                 (* No duplicates                 *)

val Bot = Neg []                           (* The absence of information    *)

fun bots n = List.tabulate(n, fn _ => Bot)

(*----------------------------------------------------------------------*)
(* Contexts, or inside-out partial term descriptions:			*)
(* Example: The context [(c2, [a2, a1]), (c1, [b2, b1])] represents	*)
(* a term description with a hole, of the form				*)
(*           c1(b1, b2, c1(a1, a2, Bot, ..., Bot), Bot, ..., Bot) 	*)
(* where the number of Bots is determined by the arity of c1 and c2.	*)
(*----------------------------------------------------------------------*)
type context = (PatCon.con * termd list) list

(*----------------------------------------------------------------------*)
(* Static matching							*)
(*----------------------------------------------------------------------*)
datatype matchresult = Yes | No | Maybe

fun staticmatch pcon (Pos(scon, _)) = 
    if PatCon.equal(pcon, scon) then Yes 
    else (case pcon of PatCon.ExCon _ => Maybe | _ => No)
  | staticmatch pcon (Neg nonset)   =
    if List.exists (fn pcon' => PatCon.equal(pcon,pcon')) nonset 
    then No
    else if PatCon.span pcon = 1 + List.length nonset 
    then Yes
    else Maybe

(*----------------------------------------------------------------------*)
(* Managing partial terms and contexts					*)
(*----------------------------------------------------------------------*)
fun addneg (Neg nonset) con = Neg(con :: nonset)
  | addneg dsc            _ = dsc

fun apply []                  dsc = []
  | apply ((con, args)::rest) dsc = 
    if PatCon.arity con = List.length args + 1 then 
        apply rest (Pos(con, List.rev(dsc :: args)))
    else
        (con, dsc :: args) :: rest

fun builddsc []                  dsc []                      = dsc
  | builddsc ((con, args)::rest) dsc ((_, _, _, sargs) :: work) = 
    builddsc rest (Pos(con, List.revAppend (args, dsc :: sargs))) work
  | builddsc _                   _   _ = Debug.fail "PatDescr.builddsc"

end