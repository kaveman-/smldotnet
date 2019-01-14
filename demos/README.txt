SML.NET Demo Programs
=====================

This directory contains various demos of SML.NET programs.

Each demo resides in its own subdirectory.

A demo can be built by cd'ing to its directory and running the
supplied script build.bat.

The demo can (typically) be executed by running the resulting *.exe file.



sort/  a simple console application that sorts a given number of
       pseudo-random integers.

trees/ a Windows application that defines a WinForm UserControl for
       displaying tree like structures. The demo illustrates the use
       of SML.NET object oriented extensions and the CLR base class
       libraries.

life/  a simple Windows application, implementing Conway's Game of Life,
       using an interface written in C# and underlying algorithm written
       in SML.NET. Illustrates interoperation between SML.NET and another
       .NET language, C#.

raytrace/ a larger Windows application, implementing a ray tracer.
       The demo use a simple interface written in C#, but the
       interpreter for the scene description language and rendering
       code is written entirely in SML.NET. The code shows off SML's
       capabilities and also illustrates interoperation between
       SML.NET and C#. The ray tracer code is based on the winning
       entry to the 3rd ICFP programming contest (see
       raytrace/ifcp/README.txt for further acknowledgements).

xq/ An ASP.NET web page providing an interface to an interpreter for a
       simple XML query language, written in SML.NET and using interop
       to read and write XML files. The code for the page itself is in
       C#, and calls into the ML code to interpret queries.

primes/ Some simple SML.NET definitions for doing asynchronous
       channel-based concurrency. These are generally useful, but are
       demonstrated here in a program which computes prime numbers
       using a concurrent version of the Sieve of Erastothenes.
       
ml-lex/ 
ml-yacc/ ML-Lex (by Andrew Appel, James Mattson and David
         Tarditi) and ML-Yacc (by Andrew Appel and David Tarditi).  
         The popular lexer and parser generators written in, and producing, 
         SML. These are essentially as in the SML/NJ distribution, but
         are provided with .smlnet files to set up entity mappings for
         compilation with SML.NET. (The contents of these directories
         are subject to their own copyright, see the file ml-yacc\COPYRIGHT 
         for details.) 

For further information on a demo, see its README.txt file.

