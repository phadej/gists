input macros;

verbatimtex
%&latex
\documentclass{article}
\usepackage{mathpazo}
\usepackage{helvet}
\usepackage{inconsolata}
\begin{document}
etex

beginfig(1);
picture q[];
path p[];
picture diag;
diag:=image(

%      1 -- 3
%      |    |
%      2 -- 4

z1=(0dimX, 0dimY);
z2=(0dimX, 1dimY);
z3=(1dimX, 0dimY);
z4=(1dimX, 1dimY);

% domain labels
q1 = thelabel(btex $a$ etex, z1);
q2 = thelabel(btex $b$ etex, z2);
q3 = thelabel(btex $s$ etex, z3);
q4 = thelabel(btex $t$ etex, z4);

for i=1 upto 4:
	draw q[i];
endfor

% arrows
p21 = z1..z2;
p43 = z3..z4;
p13 = z1..z3;
p24 = z2..z4;

p212 = clippath(sinuous(p21), q1, q2);
p432 = clippath(sinuous(p43), q3, q4);
drawarrow clippath(p212, q1, q2) withcolor blueColor;
drawarrow clippath(p432, q3, q4) withcolor blueColor;

drawarrow clippath(p13, q1, q3) shifted (0, 0.5dimY) withcolor redColor;
% drawarrow clippath(p24, q2, q4) withcolor redColor;
);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
