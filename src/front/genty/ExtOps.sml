(*======================================================================*)
(* Operations on language extension operations				*)
(*======================================================================*)
structure ExtOps :> EXTOPS =
struct

local open Ext Effect in

(*----------------------------------------------------------------------*)
(* Pretty-printing for the operation types; only for diagnostics	*)
(*----------------------------------------------------------------------*)
fun toString optype =
case optype of
  Prim x => Id.toString x
| Cast => "_cast"
| GetField => "_getfield"
| InstanceOf => "_instanceof"
| IsInst => "_isinst"
| Invoke => "_invoke"
| InvokeInterface => "_invokeinterface"
| InvokeSuper => "_invokesuper"
| New => "_new"
| NewDelegate => "_newdelegate"
| NopCast => "_nopcast"
| PutField => "_putfield"
| Synchronized => "_synchronized"
| Pure => "_pure"
| Address i => "_address_"^(Int.toString i)
| Line _ => "_line_"
(*
| Line {left={line=ll,col=lr},
	right={line=rl,col=rr},
	file=file} => "_line "^file^":"^(Int.toString ll)^"."^(Int.toString lr)^":"^
	                             (Int.toString rl)^"."^(Int.toString rr)
*)

(*----------------------------------------------------------------------*)
(* Effects for primitive ops						*)
(*----------------------------------------------------------------------*)
val primEffs = 
foldr 
(fn ((x,e),m) => Symbol.Map.insert(m, Id.fromString x, e)) 
Symbol.Map.empty
[
  (* Array load and store operations might throw out-of-bounds *)
  ("arrayload",  union(reads, throws Exns.exnSubscript)),
  ("arraystore", union(writes, throws Exns.exnSubscript)),

  (* Div and Rem operations might throw divide-by-zero *)
  ("rem", 	 throws Exns.exnDiv),
  ("div", 	 throws Exns.exnDiv),
  (*@BUG: on CLR (due to x86), div minit ~1 actually raises ArithmeticException*)

  (* New array operation might throw invalid-size; it also allocates *)
  ("newarray",   union(throws Exns.exnSize, allocs)),

  ("ref",        allocs),
 
  (":=",         writes),
  ("!",          reads),

  (* Generate new exception number *)
  ("exn",        allocs),

  (* Read the exception message *)
  ("getLocMessage", reads),

  (* Array copy reads and writes and might throw out-of-bounds *)
  ("arraycopy",  union(throws Exns.exnSize, union(reads, writes))),


  (* CLR: allocate local and return its address, use to invoke on a value class instance *)
  (* effect is conservative to defeat rewriter *)
  ("localAlloc",  any),
   
  (* CLR: conversions *)
  ("conv",  none),
  ("conv_ovf",  throws Exns.exnOverflow),
  ("conv_ovf_un", throws Exns.exnOverflow),

  (* CLR: Unsigned  div and rem operations might throw divide-by-zero *)
  ("rem_un", 	 throws Exns.exnDiv),
  ("div_un", 	 throws Exns.exnDiv),

  (* CLR: Checked signed and unsigned arithmentic *)
  ("add_ovf", 	 throws Exns.exnOverflow),
  ("mul_ovf", 	 throws Exns.exnOverflow),
  ("sub_ovf", 	 throws Exns.exnOverflow),
  ("add_ovf_un", 	 throws Exns.exnOverflow),
  ("mul_ovf_un", 	 throws Exns.exnOverflow),
  ("sub_ovf_un", 	 throws Exns.exnOverflow)
]

(*----------------------------------------------------------------------*)
(* What effect annotation is appropriate for this operation?		*)
(*----------------------------------------------------------------------*)
fun effectOf optype = 
case optype of

(* dependent on the particular primitive *)
  Prim p 	  =>
  getOpt(Symbol.Map.find(primEffs, p), none)  

(* ClassCastException *)
| Cast            => throws Exns.exnCast

(* read effect (non-null assumed) *)
| GetField        => reads

(* just a test *)
| InstanceOf      => none

(* casts but doesn't throw exception *)
| IsInst	  => none

(* anything could happen inside a method *)
(* SL: or *)
(*
| (Invoke
| InvokeInterface
| InvokeSuper)    => any
*)
| Invoke => any
| InvokeInterface => any
| InvokeSuper => any


(* anything could happen inside a constructor *)
| New             => any

(* but delegate constructors are special *)
| NewDelegate     => allocs

(* these casts do not generate code! *)
| NopCast	  => none

(* write effect (non-null assumed) *)
| PutField        => writes

(* better not allow anything around a synchronized expression *)
| Synchronized    => any

(* pure computations are at least allowed to allocate *)
| Pure            => allocs

| Address _        => any

(* Line directives have effect any *)
| Line _ => any

local open Pickle in

val file = share { empty = StringMap.empty, find = StringMap.find,
	           insert = StringMap.insert} string

val pickler =
alttag (fn Cast => 0 | GetField => 1 | InstanceOf => 2 | Invoke => 3 | InvokeInterface => 4 | InvokeSuper => 5
         | New => 6 | NewDelegate => 7 | NopCast => 8 | PutField => 9 | Synchronized => 10 | Pure => 11
         | Prim _ => 12 | IsInst => 13 | Address _ => 14 | Line _ => 15)
[
  nullary (Cast, fn Cast => ()),
  nullary (GetField, fn GetField => ()),
  nullary (InstanceOf, fn InstanceOf => ()),
  nullary (Invoke, fn Invoke => ()),
  nullary (InvokeInterface, fn InvokeInterface => ()),
  nullary (InvokeSuper, fn InvokeSuper => ()),
  nullary (New, fn New => ()),
  nullary (NewDelegate, fn NewDelegate => ()),
  nullary (NopCast, fn NopCast => ()),
  nullary (PutField, fn PutField => ()),
  nullary (Synchronized, fn Synchronized => ()),
  nullary (Pure, fn Pure => ()),
  wrap (Prim, fn Prim x => x) IdPickle.id,
  nullary (IsInst, fn IsInst => ()),
  wrap (Address, fn Address i => i) int,
  wrap (Line o (fn (ll,lr,rl,rr,file) =>
	          {left={line=ll,col=lr},
	           right={line=rl,col=rr},
	           file=file}),
        fn Line {left={line=ll,col=lr},
	         right={line=rl,col=rr},
  	         file=file} => (ll,lr,rl,rr,file))
        (quintuple(int,int,int,int,file))
]

end

end



end