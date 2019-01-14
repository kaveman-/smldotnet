Conways Game of Life
====================

Credits
-------

Server.sml is based on code from Chris Reade's book:
"Elements of Functions Programming", Addison-Wesley Publishing, 1989. 
Reproduced with kind permission of the author.


Build Instructions
------------------

To build the demo , run

build.bat

Running the Code
----------------

To run the demo, run

Client.exe

Then use the File.Go menu to start a random game of life, File.Interrupt to stop it.


Running the Code under a Debugger
---------------------------------
The demo also illustrates cross language debugging under .NET.

Start the debugger DbgClr.exe and load the solution Client.dln, if available,
or select Client.exe as the executable to Debug and load source files
Server.ml and Server.cs. 

Good places to place breakpoints (preset in Client.dln) are:

Client.cs:201:
     display = new Display();
Server.ml:7: 
     val image = factory.#MakeImage();
Server.ml:87
     fun random_row 0 = () ...
Server.ml:111
     if member (alive n) c orelse invisible c 

(in  Server.ml, the locations may vary, depending on the compiler's optimisation flags).

NB: Despite its extension, Server.ml is not a real source file, 
    but contains all the sources mentioned in Server.dll, and is used
    solely for displaying source locations in the debugger.
    This file is produced to work around a limitation in the inital release of ilasm.
    








