signature COMPILEALL =
sig

val debug : Controls.Flag

val compile : 
  string * (TyName.TyName * (Longid.longid*int)) list -> ClosConvTypes.Result -> bool

end