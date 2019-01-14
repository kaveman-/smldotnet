(*======================================================================*)
(* Paths through terms.                                                 *)
(*======================================================================*)
structure MILPath =
struct

datatype Item = 
  (* rhs of function/method defn *)
  LetFun of { recursive : bool, kind : MILTerm.FunKind option, var : Var.Var }
 
  (* branch of case *)
| CaseCon of int option
       
  (* branch of scon-case *)
| CaseSCon of Constants.constant option 

  (* branch of type-case *)
| TypeCase of MILTy.Type option        

(* The head of the list is the deepest node in the corresponding term *)
type Path = Item list

end

