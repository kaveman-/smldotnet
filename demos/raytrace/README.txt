A Standard ML Ray Tracer
=======================


Background
----------

Client.cs defines a simple graphical interface to an off-line
raytracer, implemented as a Standard ML library called Server.dll.

Server.sml interface to code in directory .\ifcp, which contains an
almost (literal) port, from OCaml to Standard ML, of the winning entry
to the The Third Annual ICFP Programming Contest.  The contest home
page is at:

http://www.cs.cornell.edu/icfp/

The original task was to implement a ray tracer that produces .ppm
image files from .gml input files (GML is a toy scene description
language).  See the original web site for further information and a
detailed task description.

The code in this directory adds a graphical front-end to the raytracer
that displays images as they are traced (it does not, however, produce
.ppm files).  The front-end is written in C# (see file Client.cs).
The C# code interfaces to the Standard ML raytracer via the SML.net
glue code in Image.sml, Classes.sml, and Server.sml.

Directory ..\gml contains sample image files obtained from the contest
web site. These are actually the cpp'ed versions of the original .gml
files in http://www.cs.cornell.edu/icfp/allGmlTests.tar.gz.

See the .\icfp\README.txt for more details.

Acknowledgments
---------------

All credit for the code goes to the original authors, the members of
Team PL6: Jerome Vouillon, Haruo Hosoya, Eijiro Sumii and Vladimir
Gapeuyev.

Team PL6's home page (with original OCaml code) is available at:

http://www.cis.upenn.edu/~sumii/icfp/.

Many thanks to Stephen Weeks for the initial port of the code to Standard
ML, and John Reppy for tweaking it for SML/NJ.

Minor modifications have been made by the SML.NET authors.

Build Instructions
------------------

To build the demo, run

build.bat

Running the Code
----------------

To run the demo, run

Client.exe

Then use the File.Open menu to select a .gml file to trace and render
the images define in the .gml to the screen.












