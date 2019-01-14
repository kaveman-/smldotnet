(*======================================================================*)
(* Long identifiers. Used for						*)
(* - ML long identifiers (e.g. structure identifiers)			*)
(* - external identifiers (e.g. fully-qualified class names)   		*)
(*======================================================================*)
structure Longid :> LONGID =
struct
   type longid = Symbol.symbol list

  (* Insert "." between structure components of path *)
  fun toString [] = ""
    | toString [id] = Id.toString id
    | toString (strid::longid) = 
      Id.toString strid ^ "." ^ toString longid

  structure Key = struct
    type ord_key = longid
    val compare = Compare.list Id.Key.compare
  end
  structure Set = SetFn(Key)
  structure Map = MapFn(Key)

  fun equal ([],[]) = true
    | equal (x::xs, y::ys) = Id.equal(x,y) andalso equal(xs,ys)
    | equal _ = false
end


