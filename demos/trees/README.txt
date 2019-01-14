A Graphical Tree Viewer
=======================

This demo defines and demonstrates a graphical control, suitable for
displaying trees-like structures.  At its heart lies an elegant
algorithm for laying out and drawing trees, originally described in:

   "Functional Pearls: Drawing Trees", A. J. Kennedy,                
    Journal of Functional Programming 6(3), May 1996, pp 527-534     

The code for the algorithm is in subdirectory .\jfp.


Build Instructions
------------------

Running

build.bat

builds

WinApp.exe

To run the application, execute WinApp.exe from the command-line.


KNOWN BUGS: 
-----------
drawing a large tree (such as wide or deep directory) and
resizing the application crashes the program, apparently because of
improper resource management using "Dispose()".
