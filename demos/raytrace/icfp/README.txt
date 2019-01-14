A Standard ML Ray Tracer
=======================


Background
----------

This directory contains an almost (literal) port, from OCaml to
Standard ML, of the winning entry to the The Third Annual ICFP
Programming Contest.  The contest home page is at:

http://www.cs.cornell.edu/icfp/

The task was to implement a ray tracer that produces .ppm image files
from .gml input files (GML is a toy scene description language).  See
the original web site for further information and a detailed task
description.

Directory ..\gml contains sample image files obtained from the contest
web site. These are actually the cpp'ed versions of the original .gml files
in http://www.cs.cornell.edu/icfp/allGmlTests.tar.gz.

Acknowledgments
---------------

All credit for the code goes to the original authors, the members
of Team PL6: Jerome Vouillon, Haruo Hosoya, Eijiro Sumii and Vladimir
Gapeuyev.

Team PL6's home page (with original OCaml code) is available at:

http://www.cis.upenn.edu/~sumii/icfp/.

Many thanks to John Reppy for the initial port of the code to Standard
ML of New Jersey.

Minor modifications have been made by the SML.NET authors.

Build Instructions
------------------

You can build a command line raytracer called 

Standalone.exe

by running

build.bat

Standalone.exe takes its .gml code from standard input.


Running the Code
----------------

Simply redirect a file containing gml code to Standalone.exe, eg:

Standalone < ..\gml\chess.gml

then view the resulting *.ppm files (http://www.cs.cornell.edu/icfp/
contains links to appropriate viewers).

Note that a single .gml file may produce zero or more .ppm files of
unrelated names.

Modifications from John Reppy's Port
------------------------------------

o modified to directly use the Standard ML Basis library

o modified to produce ASCII, not binary, format .ppm files

o simplified and reorganized for demonstration purposes

Notes
-----

the files 

..\gml\*.gml.disabled

are renamed because they exercise bugs (dice.gml.disabled) or are simply meant to fail.










