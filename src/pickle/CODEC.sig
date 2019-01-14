signature CODEC = sig
  (* The type of encodings (e.g. a stream of bytes) *)
  type State

  (* Initial state *)
  val initial : State

  (* Exception raised when encoding *)
  exception Pickle of string

  (* Exception raised when decoding *)
  exception Unpickle of string

  (* codec n : Encode/Decode a natural number i with 0 <= i <= n *)
  (* Assume even distribution *)
  (* e.g. codec 0w1 is booleans; codec 0w255 is bytes *)
  val codec : Word32.word -> ((Word32.word*State -> State) * (State -> Word32.word*State))

  (* codec' freqs : Encode/Decode a natural number i with 0 <= i < length freqs *)
  (* Assume distribution given by freqs, a list of counts *)
  val codec' : Word32.word list -> ((Word32.word*State -> State) * (State -> Word32.word*State))
end