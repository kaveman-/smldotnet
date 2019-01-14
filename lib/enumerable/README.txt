The Enumerable structure provides List-like operations for .NET
classes which implement the System.Collections.IEnumerable
interface. See the ENUMERABLE signature for the implemented operations
(whose semantics should be fairly obvious by reference to the List
structure in the Standard Basis).

To use the library in your own code, just include
 source:%smlnetpath%\lib\enumerable
in your .smlnet file (or pass it as a command-line option).

The application Test exercises a little of the functionality of the
Enumerable structure. You can build it with
  smlnet Test


