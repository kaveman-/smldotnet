(*======================================================================*)
(* Extensions to SML and MIL to support					*)
(*   (a) primitive operations (add, mul, etc.)				*)
(*   (b) interop constructs (invocation,field access,obj alloc,casts)	*)
(*   (c) the "pure" annotation						*)
(*======================================================================*)
structure Ext =
struct

(*----------------------------------------------------------------------*)
(* Language extensions, as seen in typed SML terms and MIL.             *)
(* Some do not correspond to actual source syntax.			*)
(*----------------------------------------------------------------------*)
datatype OpType = 
  (* Primitive operations *)
  Prim of Syntax.symbol

  (* Next, some interop constructs *)
| Cast 		(* Cast *)
| GetField 	(* Static/instance field access *)
| InstanceOf	(* Boolean instance-of test *) 
| IsInst	(* Cast that results in null on failure *)
| Invoke	(* Ordinary method invocation *) 
| InvokeInterface	(* Interface invocation *)
| InvokeSuper 	(* ? *)
| New		(* New object + constructor call *)
| NewDelegate   (* New delegate object *)          
| NopCast	(* No-code cast (upcast or C -> C option) *) 
| PutField	(* Static/instance field update *)
| Synchronized	(* Locking *)
| Address of int    (* take the address of a reference *)
  (* Finally, an annotation for pure computations *)
| Pure 
  (* Debug information *)
| Line of {left:{line:int,col:int},right:{line:int,col:int}, file: string}



end