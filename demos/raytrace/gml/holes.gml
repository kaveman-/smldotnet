% holes.gml
%
% OUTPUTS: holes.ppm
%
% The sea of holes (all we need is a yellow submarine)
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





{ black 1.0 0.0 1.0 } /surf1
{ white 1.0 0.0 1.0 } /surf2

{ /v /u /face
  u frac absf apply 0.5 subf /uu
  v frac absf apply 0.5 subf /vv
  uu uu mulf vv vv mulf addf sqrt 0.3 lessf
  surf1
  surf2
  if
} plane 3.0 uscale -20.0 rotatex 0.0 -5.0 0.0 translate /holes

% render the holes with just ambient lighting
1.0 1.0 1.0 point	% ambient
[]			% lights
holes
1
90.0
320 200
"holes.ppm"
render

% now add a submarine

yellow matte apply sphere /s

s 4.0 1.0 1.3 scale
s 0.8 uscale -1.0 0.5 0.0 translate union /sub

sub 25.0 rotatey 0.0 1.0 5.0 translate
holes union /scene

% directional light
1.0 -1.0 1.0 point
1.0 1.0 1.0 point light /l

0.3 0.3 0.3 point
[l]
scene
1
90.0
320 200
"submarine.ppm"
render

