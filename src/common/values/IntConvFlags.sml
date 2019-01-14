structure IntConvFlags=
struct
   datatype Base=Hex|Decimal
   datatype Kind=Unsigned | Signed of bool
   (* The RTInt.fromString and RTLong.fromString functions are parameterised
      by a variable of type Base and a variable of type Kind.

      The variable of type Base indicates the radix of the string, decimal
      or hexadecimal.  For hexadecimal, the digits above 9 must be lower case.

      The variable of type Kind gives signing information (the string itself
      should not contain a sign, only digits).  Let N be the number represented
      by the digits of the string, and K be the number of bits needed to
      represent the target Java type (K=32 for RTInt and K=64 for RTLong).
      For k=Unsigned, N should be in [0,2^K) and the K bits of the
      resulting Java integer are exactly those of N.  For K=Signed(false),
      N should be in [0,2^(K-1)) and the Java number will equal N.  For
      K=Signed(true), N should be in [0,2^(K-1)] and the Java number will
      equal -N.  If N is not in the appropriate interval the fromString
      functions return NONE.
      *)
end
