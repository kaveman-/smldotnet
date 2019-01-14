signature ELABSTATE = 
sig

type 'a ElabResult = 
  'a * Error.Error list * SMLTy.DatEnv * SMLTy.Realisation
type ErrorArg = string * SMLTy.Type

val error       : Error.Error * ErrorArg list -> unit
val noSeriousErrors   : unit -> bool
val freshTyVar  : TyVar.Sort -> TyVar.TyVar
val freshType   : unit -> SMLTy.Type
val freshMono   : unit -> SMLTy.Type
val openRecType : (Syntax.symbol*SMLTy.Type) list -> SMLTy.Type

val freshTyName : Syntax.longid * TyName.EqStatus -> TyName.TyName
val freshClass  : Syntax.longid -> TyName.TyName
val freshRecTyNames: (Syntax.longid * TyName.EqStatus) list -> TyName.TyName list
val makeRenaming: 
  Syntax.longid * TyName.Set.set -> TyName.Renaming * TyName.Set.set

val freshVar    : unit -> Syntax.symbol 
val addDE       : SMLTy.DatEnv -> unit
val appRenamingDE: TyName.Renaming -> unit 
val addRealisation : SMLTy.Realisation -> unit

val getStamp        : unit -> TyName.Supply
val getEntity       : unit -> Entity.Ref 

val runelab         : Entity.Ref -> (unit -> 'a) -> 'a ElabResult

end