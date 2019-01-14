(*======================================================================*)
(* Revised class handle interface.					*)
(* This provides the bare minimum for identification of classes.	*)
(*======================================================================*)
signature CLASSHANDLE = 
sig
   type Handle = Symbol.symbol list
   val name : Handle -> UString.t
   val unknown : UString.t -> Handle
   val equal : Handle * Handle -> bool
   val superclass : Handle -> Handle option
   val is_interface : Handle -> bool option
   val maybe_subclass : Handle * Handle -> bool
   (* maybe_subclass(X,Y) should return true if X is a subclass of Y *)  

   val class_handle_toString:Handle->string
   (* class_handle_toString outputs a string representation of a class handle.
      It should only be used for debugging purposes. *)

   (* The root class *)
   val object:Handle

   (* Root class for arrays *)
   val toparray:Handle

   (* The string class: required because string constants are built-in *)
   val string:Handle

   (* The base class for exceptions (= exn) *)
   val Exception:Handle
   val throwable:Handle

   (* The base class for attributes (CLR only) *)
   val attribute:Handle

   (* The base class for enums (CLR only) *)
   val enum:Handle

   (* The base classes for delegate(s) (CLR only) *)
   val delegate:Handle
   val multicastDelegate:Handle

(*
   (* Some useful interfaces *)
   val cloneable:Handle
   val serializable:Handle
*)
   

end