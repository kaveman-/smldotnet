(*======================================================================*)
(* Make a whole project							*)
(*======================================================================*)
signature MAKE =
sig

  val opts : string list ref

  (* Compile and link the current project *)
  (* [make false] relinks if necessary (files have changed or target doesn't exist) *)
  (* [make true] relinks regardless *)
  val make : bool -> bool


  (* Extend export list *)
  val setExports: (string*string option) list -> OS.Process.status

  (* Return name of class containing main, if a unique such class exists *)
  val getMainClass : unit -> string option

end