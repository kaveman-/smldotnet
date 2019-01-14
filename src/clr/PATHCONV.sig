(* Path conversion *)
signature PATHCONV =
sig
  val toInternal : string -> string
  val toExternal : string -> string
end
