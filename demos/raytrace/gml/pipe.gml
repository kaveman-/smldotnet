% pipe.gml
%
% OUTPUTS: pipe.ppm
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
0.8 0.8 0.8 point 0.8 0.0 1.0 const-surface apply plane /p

% background plane
0.4 0.5 0.6 point matte apply plane
  -90.0 rotatex 0.0 0.0 500.0 translate /background

blue matte apply cylinder
  2.0 20.0 2.0 scale

white 0.0 1.0 10.0 const-surface apply cylinder
  1.8 24.0 1.8 scale
  0.0 -2.0 0.0 translate
  difference
  90.0 rotatex /pipe

red 0.9 0.2 2.0 const-surface apply sphere 0.2 uscale /ball1
yellow 0.9 0.2 2.0 const-surface  apply cube 0.5 uscale /box
green 0.9 0.2 2.0 const-surface  apply cone 0.4 uscale /Cone
white 0.4 0.8 2.0 const-surface apply sphere 0.2 uscale /ball2

background
pipe union
ball1 0.3 -0.05 6.0 translate union
ball2 0.5 -0.25 1.0 translate union
box -0.5 0.2 13.0 translate union
Cone -15.0 rotatey -1.0 -0.3 11.0 translate union
0.0 0.0 4.0 translate
p 0.0 -5.0 0.0 translate union
/scene

				% directional light
0.8 -1.0 0.4 point		  % direction
0.8  0.8 0.8 point light /l1	  % directional light

0.0 0.0 6.0 point
0.9 0.9 0.9 point pointlight /l2

0.8 0.8 0.8 point		  % ambient light
[ l1 l2 ]				  % lights
scene				  % scene to render
3				  % tracing depth
60.0				  % field of view
400 300				  % image wid and height
"pipe.ppm"		  % output file
render
