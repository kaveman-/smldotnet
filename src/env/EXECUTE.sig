(*======================================================================*)
(* Execution of managed code.						*)
(*======================================================================*)
signature EXECUTE = 
sig

(* Execute a managed program with specified arguments *)
val run : { program:string, args:string } -> OS.Process.status

(* Execute a helper from the SMLNETBIN\bin directory *)
val runHelper : { program:string, args:string, out:string} ->
  OS.Process.status

end