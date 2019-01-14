% fractal.gml
%
% OUTPUTS fractal.ppm
%
% Fractal thingie with spheres.
%

 







%%%%%%%%%% include surface.ins %%%%%%%%%%

 









%%%%%%%%%% include util.ins %%%%%%%%%%

{ /x } /pop		% pop a stack item
{ /x x x } /dup		% duplicate a stack item

% pop n items off the stack
{ /n
  { /self /i
    i 1 lessi
    { }
    { /x i 1 subi self self apply }
    if
  } /loop
  n loop loop apply
} /popn

% dot product
% ... v2 v1  dot  ==> ... r
{ /v1 /v2
  v1 getx v2 getx mulf
  v1 gety v2 gety mulf addf
  v1 getz v2 getz mulf addf
} /dot

% integer absolute value
{ /i i 0 lessi { i negi } { i } if } /absi

% floating-point absolute value
{ /f f 0.0 lessf { f negf } { f } if } /absf

% normalize
% ... v1  normalize  ==> ... v2
{ /v
  1.0 v v dot apply sqrt divf /s	% s = sqrt(1.0/v dot v)
  s v getx mulf				% push s*x
  s v gety mulf				% push s*y
  s v getz mulf				% push s*z
  point					% make normalized vector
} /normalize

% addp
{ /v2 /v1
  v1 getx v2 getx addf
  v1 gety v2 gety addf
  v1 getz v2 getz addf point
} /addp

% subp
{ /v2 /v1
  v1 getx v2 getx subf
  v1 gety v2 gety subf
  v1 getz v2 getz subf point
} /subp

% mulp
{ /v2 /v1
  v1 getx v2 getx mulf
  v1 gety v2 gety mulf
  v1 getz v2 getz mulf point
} /mulp

% negp
{ /v
  v getx negf
  v gety negf
  v getz negf point
} /negp


% A simple pseudo-random number generator (from Graphics Gems II; p. 137)
% We have to do the computation in FP, since it overflows in integer arithmetic.
{ real 25173.0 mulf 13849.0 addf 65536.0 divf frac 65536.0 mulf floor } /random

% A random number in [0..1].
%
% seed  randomf  ==> f seed
{ 
  random apply /seed
  seed real 65535.0 divf seed
} /randomf

%%%%%%%%%% util.ins %%%%%%%%%%




 







%%%%%%%%%% include colors.ins %%%%%%%%%%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

% ... <level>  grey  ==>  <color>
{ clampf /level level level level point } /grey

%%%%%%%%%% colors.ins %%%%%%%%%%




% ... <color> matte  ==>  ... <surface>
{ /color
  { /v /u /face
    color 1.0 0.0 1.0
  }
} /matte

% ... <color> <kd> <ks> <n>  ==>  ... <surface>
{ /n /ks /kd /color
  { /v /u /face
    color kd ks n
  }
} /const-surface

%%%%%%%%%% surface.ins %%%%%%%%%%





0.4 0.5 0.6 point 1.0 0.1 1.0 const-surface apply plane /p

{ /col
  { /v /u /face
    col
    0.1 0.99 6.0
  } sphere 0.9 uscale
} /mksphere

[
  0.5 0.7 0.9 point
  0.5 0.9 0.5 point
  0.6 0.6 0.7 point
  1.0 0.7 0.5 point
  0.9 1.0 0.6 point
  1.0 0.5 0.3 point
  1.0 0.8 0.9 point
  1.0 1.0 0.6 point
  1.0 1.0 1.0 point
] /colors

{ 1 addi colors length modi } /incrmod

{
  /self /col /depth /base
  depth 0 eqi
  { colors col get base apply 0.9 uscale }
  { 
    col incrmod apply /col
    colors col get base apply
    col incrmod apply /col
    base depth 1 subi col self self apply 2.5 0.0 0.0 translate union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 60.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 120.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 180.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 240.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 300.0 rotatez union

    col incrmod apply /col
    base depth 1 subi col self self apply
    3.0 0.0 0.0 translate 90.0 rotatey union
    col incrmod apply /col
    base depth 1 subi col self self apply
    3.0 0.0 0.0 translate -90.0 rotatey union

    1.0 3.0 divf uscale
  }
  if
} /rec

mksphere 3 0 rec rec apply 30.0 rotatex 40.0 rotatey
0.8 uscale 0.0 0.3 0.5 translate

p 0.0 -1.0 0.0 translate union
0.0 -0.2 0.3 translate
 /scene

				% directional light
0.8 -1.0 0.4 point		  % direction
1.0  1.0 1.0 point light /l1	  % directional light

0.4 0.4 0.4 point		  % ambient light
[ l1 ]				  % lights
scene				  % scene to render
3				  % tracing depth
100.0				  % field of view
300 200				  % image wid and height
"fractal.ppm"			  % output file
render

