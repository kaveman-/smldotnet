structure TransInterop =
struct

(*----------------------------------------------------------------------*)
(* What's are the method argument and result types for this function    *)
(* type?								*)
(*----------------------------------------------------------------------*)
fun transMethodType milty =
let
  val SOME ([argty], cty) = MILTy.fromArrow milty
  val argtys = 
    case MILTy.fromProd argty of
      SOME tys => tys
    | NONE => [argty]
                  
  val (_,[ty]) = MILTy.fromCmp cty
  val restyopt = 
    case MILTy.fromProd ty of
      SOME [] => NONE
    | _ => SOME ty
in
  (argtys, restyopt)
end

end



