structure Interrupt =
struct

val oldaction = ref (NONE : Signals.sig_action option)

(*----------------------------------------------------------------------*)
(* Message printed when control-C is typed.				*)
(*----------------------------------------------------------------------*)
val intMessage = "\nInterrupt\n"

fun init () =
(
  oldaction := SOME (Signals.inqHandler Signals.sigINT);
  SMLofNJ.Cont.callcc (fn k => ignore (Signals.setHandler (Signals.sigINT, 
      Signals.HANDLER (fn (signal, n, k') => 
      (print intMessage; k)))))
)

fun finish () = ignore(Signals.setHandler (Signals.sigINT, valOf (!oldaction)))

end
