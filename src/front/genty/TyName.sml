(*======================================================================*)
(* ML type names (Section 4.1, p16 Defn)				*)
(*======================================================================*)
structure TyName :> TYNAME = 
struct

open ListOps
val showStamps = Controls.add false "showStamps"
val showTyNameSort = Controls.add false "showTyNameSort"

(* Equality status of type names; see sig for details *)
datatype EqStatus = NotEq | RefEq | Eq

(*----------------------------------------------------------------------*)
(* An ML type name consists of:						*)
(*   1. The entity in which the tyname was generated.                   *)
(*      For a runtime-imported type, this is an assembly.		*)
(*   2. An integer identifier unique to this tyname within that entity. *)
(*      For imported types, this is zero.				*)
(*   3. The equality status of the tyname.                              *)
(*   4. A longid:							*)
(*      - for ML type names, this is just used for pretty-printing      *)
(*      - for imported types, this is its fully-qualified name		*)
(*   5. Whether this is a class type (internal or external)		*)
(*   6. The nesting depth, or #number of enclosing class of the class   *)
(*----------------------------------------------------------------------*)
datatype TyName = 
  TyName of 
  {
    entity   : Entity.Ref,
    tag      : int,
    equality : EqStatus,
    longid   : Longid.longid,
    isClass  : bool,
    depth : int
  }

type Supply = Entity.Ref * int

(*----------------------------------------------------------------------*)
(* ORD structure							*)
(*----------------------------------------------------------------------*)
structure Ord =
  struct
    type ord_key = TyName
    fun compare 
      (TyName {entity=e1, tag=i1, longid=id1, depth=d1,...}, 
       TyName {entity=e2, tag=i2, longid=id2, depth=d2,...}) =
      (case Entity.Set.Key.compare (e1,e2) of
        EQUAL => 
        (case Int.compare (i1, i2) of
          EQUAL =>
          (case e1 of (Entity.Assembly,_) =>
	     (*@TODO: review - depth significant for external classes only *)
             (case Int.compare(d1,d2) of
		 EQUAL => Compare.list Symbol.Key.compare (id1,id2)
	       | other => other)
	   | _ => EQUAL)
        | other => other)
      | other => other)
  end

(*----------------------------------------------------------------------*)
(* Sets of type names and finite maps from type names			*)
(*----------------------------------------------------------------------*)
structure Set = SetFn(Ord)
structure Map = MapFn(Ord)
type Renaming = TyName Map.map

(*----------------------------------------------------------------------*)
(* Pickling and unpickling						*)
(*----------------------------------------------------------------------*)
local open Pickle in

val equalityPickler = 
  alttag (fn NotEq => 0 | RefEq => 1 | Eq => 2) 
  [
    wrap (fn () => NotEq, fn NotEq => ()) unit,
    wrap (fn () => RefEq, fn RefEq => ()) unit,
    wrap (fn () => Eq, fn Eq => ()) unit
  ]

val pickler =
wrap (fn (e,t,q,l,c,d) => TyName{entity=e,tag=t,equality=q,longid=l,isClass=c,depth=d},
      fn TyName{entity,tag,equality,longid,isClass,depth} => (entity,tag,equality,longid,isClass,depth))
(sextuple (EntityOps.pickler, int, equalityPickler, IdPickle.longid, bool,int))

fun mapPickler p =
  wrap (foldr Map.insert' Map.empty, Map.listItemsi) (list (pair (pickler, p)))

end


fun rename r tyname =
case Map.find(r, tyname) of
  NONE => tyname
| SOME tyname' => tyname'

fun longid ss = map Id.fromString ss

(*----------------------------------------------------------------------*)
(* Create a tyname associated with an external class, struct or prim	*)
(*----------------------------------------------------------------------*)
fun externalEq (assembly,id,depth) = 
  TyName { entity = (Entity.Assembly,assembly), equality = Eq, longid = id, 
    tag = 0, depth = depth, isClass = true  }

fun externalValEq (assembly,id,depth) =
  TyName { entity = (Entity.Assembly,assembly), equality = Eq, longid = id, 
    tag = 0, depth = depth, isClass = false }

fun external (assembly,id,depth) = 
  TyName { entity = (Entity.Assembly,assembly), equality = NotEq, longid = id, 
    tag = 0, depth = depth, isClass = true }

fun externalVal (assembly,id,depth) = 
  TyName { entity = (Entity.Assembly,assembly), equality = NotEq, longid = id, 
    tag = 0, depth = depth, isClass = false }

(*----------------------------------------------------------------------*)
(* Equality on type names						*)
(*----------------------------------------------------------------------*)
fun eq (tyname1,tyname2) = Ord.compare(tyname1, tyname2) = EQUAL

(*----------------------------------------------------------------------*)
(* Type name supply functions						*)
(*----------------------------------------------------------------------*)
fun initial entity = (entity,1)
fun fresh (longid, eq) (entity,i) = 
  (TyName { entity = entity, tag = i, equality = eq, longid = longid, depth = 0, isClass = false }, 
  (entity,i+1))

fun freshClass longid (entity,i) = 
  (TyName { entity = entity, tag = i, equality = NotEq, longid = longid, depth = 0, isClass = true }, 
  (entity,i+1))

fun freshen longid' (TyName { entity, tag, equality, longid, depth, isClass },
            (entity',tag')) =
  (TyName {entity = entity', tag = tag', equality = equality, depth = depth, isClass = isClass,
           longid = longid' @ [List.last longid] }, (entity',tag'+1))

fun freshRec pairs (entity,i) = 
  (mapi (fn (j,(longid,equality)) => 
    TyName { entity = entity, tag = i+j, longid = longid,
             equality = equality, isClass = false, depth = 0 }) pairs, (entity,i+length pairs))

fun newEquality (TyName { entity, tag, equality, longid, isClass, depth}) equality' =
  TyName { entity = entity, tag = tag, equality = equality',longid = longid, isClass = isClass, depth = depth }

fun temp (longids, equality) = mapi (fn (i, longid) =>
  TyName { entity = (Entity.Str, Id.fromString "$temp"),
           tag = ~i, equality = equality, longid = longid, 
	   isClass = false, depth=0 }) longids

fun depth (TyName { depth, ... }) = depth

fun entity (TyName { entity, ... }) = entity

fun longid (TyName { longid, ... }) = longid

fun equality (TyName { equality, ... }) = equality

fun isClass (TyName { isClass, ... }) = isClass

fun isExternal (TyName { entity = (Entity.Assembly,_), ... }) = true
  | isExternal _ = false

fun fromExternal (TyName { entity = (Entity.Assembly,assembly), longid, depth,...}) =
    SOME (assembly,longid,depth)
  | fromExternal _ = NONE

(* @todo akenn: externals *)
fun hash 
  (TyName { entity, tag, equality, longid, ... }) =
    Gen.combine (Word.fromInt tag + 0w1, Symbol.HashKey.hashVal (#2 entity))

(*----------------------------------------------------------------------*)
(* Was a tyname generated earlier than the point specified?     	*)
(*----------------------------------------------------------------------*)
fun earlier 
  (TyName { entity, tag, ...}, (entity', tag')) = 
  not (EntityOps.eq (entity, entity')) orelse tag<tag'

(*----------------------------------------------------------------------*)
(* Display a type name							*)
(*----------------------------------------------------------------------*)
fun toString (tyname as TyName { entity,tag,longid,equality,... }) = 
  (if Controls.get showTyNameSort andalso equality=Eq then "'" else "")
 ^
  ((*if eq(tyname, boolTyName) then "bool"
   else if eq(tyname, exnTyName) then "exn"
   else if eq(tyname, listTyName) then "list"
   else if eq(tyname, optionTyName) then "option"
   else if eq(tyname, orderTyName) then "order"
   else if eq(tyname, stringTyName) then "string"
   else*) Longid.toString longid) 
  ^
  (if (Controls.get showStamps orelse null longid) then "(" ^ 
     (EntityOps.toString entity) ^ "#" ^ Int.toString tag ^ ")" else "")

end






