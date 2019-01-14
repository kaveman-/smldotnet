% cone-fractal.gml
%
% OUTPUTS: cone-fractal.ppm
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





% ground plane
0.4 0.5 0.6 point 1.0 0.1 1.0 const-surface apply plane /p

% background plane
0.6 0.5 0.5 point matte apply plane
  -90.0 rotatex 0.0 0.0 500.0 translate /background

3.0 sqrt /sqrt3

% compute the height of a triangle/cone from the length of its side.
% ht = sz * sqrt(3)
{ sqrt3 mulf } /triHt

[
  red green blue yellow cyan magenta
] /colors

{ 1 addi colors length modi } /incrmod

{ /sz /color
  color 0.2 0.9 10.0 const-surface apply
  cone
  1.0 -1.0 1.0 scale 0.0 1.0 0.0 translate	% flip so base is at y=0.
  sz sz triHt apply sz scale
} /mkCone

{ /self /depth /sz /col
  depth 0 eqi
  { colors col get sz mkCone apply col incrmod apply }
  { sz triHt apply 0.5 mulf /halfHt
    sz 0.5 mulf /halfSz
  % top triangle
    col halfSz depth 1 subi self self apply /col
    0.0 halfHt 0.0 translate /tri1
  % bottom left
    col halfSz depth 1 subi self self apply /col
    0.0 0.0 halfHt translate
    120.0 rotatey /tri2
  % bottom right
    col halfSz depth 1 subi self self apply /col
    0.0 0.0 halfHt translate
    -120.0 rotatey /tri3
  % bottom back
    col halfSz depth 1 subi self self apply /col
    0.0 0.0 halfHt translate /tri4
  % form the composite image
    tri1 tri2 union tri3 tri4 union union col
  } if
} /genFractal

0 2.0 3 genFractal genFractal apply /col
10.0 rotatey
0.0 -2.5 5.0 translate
p 0.0 -5.0 0.0 translate union
-15.0 rotatex
background union
 /scene

				% directional light
0.8 -1.0 0.4 point		  % direction
0.8  0.8 0.8 point light /l1	  % directional light

0.0 2.0 6.0 point
0.9 0.9 0.9 point pointlight /l2

0.4 0.4 0.4 point		  % ambient light
[ l1 l2 ]			  % lights
scene				  % scene to render
3				  % tracing depth
90.0				  % field of view
300 200				  % image wid and height
"cone-fractal.ppm"		  % output file
render

