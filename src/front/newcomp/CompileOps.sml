structure CompileOps =
struct

val entrypoint = Id.fromString "entrypoint"

val assemblySym = Id.fromString "assembly"
val sealedSym = Id.fromString "sealed"
val noinliningSym = Id.fromString "noinlining"

(* a class pseudo-flag to indicate (member) augmentation of an already declared class *)
val augmentation = Id.fromString "augmentation"

val privatesealed = 
  Symbol.Set.addList(Symbol.Set.empty, [sealedSym, Id.privateSym])

val private =
  Symbol.Set.addList(Symbol.Set.empty, [Id.privateSym])

val public = 
  Symbol.Set.addList(Symbol.Set.empty, [Id.publicSym])

val publicstatic = 
  Symbol.Set.addList(Symbol.Set.empty, [Id.publicSym, Id.staticSym])

val publicsealed = 
  Symbol.Set.addList(Symbol.Set.empty, [Id.publicSym, Id.sealedSym])

val publicstaticinitonly = 
  Symbol.Set.addList(Symbol.Set.empty, [Id.publicSym, Id.staticSym, Id.initonlySym])

val publicstaticliteral = 
  Symbol.Set.addList(Symbol.Set.empty, [Id.publicSym, Id.staticSym, Id.fromString "literal"])

val publicstaticinitonly = 
  Symbol.Set.addList(Symbol.Set.empty, [Id.publicSym, Id.staticSym, Id.initonlySym])

val publicspecialnamertspecialname = Symbol.Set.addList(Symbol.Set.empty, [Id.publicSym, Id.fromString "specialname", Id.fromString "rtspecialname"])

val assemblystatic =
  Symbol.Set.addList(Symbol.Set.empty, [assemblySym, Id.staticSym])

val assembly =
  Symbol.Set.addList(Symbol.Set.empty, [assemblySym])

val assemblyfinal =
  Symbol.Set.addList(Symbol.Set.empty, [assemblySym, Id.finalSym])

val GCSafePointMethod = Id.fromString ".GCSafePoint"



val assemblystaticnoinlining =  Symbol.Set.addList(Symbol.Set.empty, [assemblySym, Id.staticSym, noinliningSym])

val useOverrides = Controls.add true "codegen.useOverrides"
val verifiable = Controls.add true "codegen.verifiable" 

end