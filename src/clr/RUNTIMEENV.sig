(*======================================================================*)
(* Info about the runtime environment					*)
(*======================================================================*)
signature RUNTIMEENV =
sig

(* Set up the compiler/runtime environment: 
     determine compiler binary directory
     determine runtime version
     determine runtime system directory
     check presence of compiler tools (getmeta, clslist, getsysdir)
     check presence of runtime tools (ilasm, peverify)
  If there are any problems, print to console and return false
*)
val setup : {SMLNETPATH:string,FrameworkDir:string,FrameworkVersion:string} option -> bool 

(* Query functions for info picked up by setup. Exception will be raised if setup hasn't been run *)

(* The runtime system directory (e.g. C:\windows\microsoft.net\framework\v1.1.4322) *)
val getSysDir : unit -> string

(* The runtime version (e.g. 1.1.4322.573) *)
val getVersion : unit -> string 

(* Are we using the compiler-shipped version of ilasm? *)
val getCompilerIlasm : unit -> bool

(* The compiler directory (e.g. C:\smlnet) *)
val getCompilerDir : unit -> string

(* The compiler binary directory (e.g. C:\smlnet\bin) *)
val getCompilerBinDir : unit -> string

(* The compiler tool directory (e.g. C:\smlnet\bin\v1.0.3705) *)
val getCompilerToolDir : unit -> string

(* Full path to getmeta tool (e.g. C:\smlnet\bin\getmeta.exe) *)
val getGetmetaFileName : unit -> string

(* Full path to clslist tool (e.g. C:\smlnet\bin\clslist.exe) *)
val getClslistFileName : unit -> string

(* Full path to ilasm tool (e.g. C:\windows\microsoft.net\framework\v1.1.4322\ilasm.exe) *)
val getIlasmFileName : unit -> string

(* Full path to peverify tool (e.g. C:\windows\microsoft.net\framework\v1.1.4322\peverify.exe) *)
(* If it doesn't exist, return NONE *)
val getPeverifyFileName : unit -> string option

(* Run a helper program *)
val runHelper : { program : string, args : string, out : string } -> OS.Process.status

(* Run a managed program *)
val run : { program : string, args : string } -> OS.Process.status

end

