(*======================================================================*)
(* Turn a tree layout into a sequence of draw/label commands		*)
(*======================================================================*)
signature Draw =
sig

type 'a Interface =
  {
    line : ((int*int) * (int*int)) -> unit,
    label : 'a * (int*int) -> Tree.Extent,
    dropbelow : int,    (* Line drop from below text *)
    dropabove : int     (* Line drop from above text *)
  }

(* Now returns list of nodes with top-left, bottom-right coords e.g.
   for constructing image map *)
val draw1 : 
  'a Interface -> real*int*Tree.Extent list -> ('a*real) Tree.Tree -> 
  ('a * ((int*int)*(int*int))) list

val draw2 : 
  'a Interface -> real*int*Tree.Extent list -> ('a*real) Tree.Tree -> 
  ('a * ((int*int)*(int*int))) list

end

