input opticmacros;

verbatimtex
%&latex
\documentclass{article}
\usepackage{mathpazo}
\usepackage{helvet}
\usepackage{inconsolata}
\begin{document}
etex

% 1 --- 2
% |     |
% 3 --- 4

beginfig(1);
picture diag;
path p[];
pair u[];
diag:=image(

label.top(btex $\mathit{re\ Iso}$ etex, d0);

% Labels
draw (thelabel.rt(btex $\mathit{b}$ etex, (0,0)) rotated 180) shifted d1;
draw (thelabel.lft(btex $\mathit{t}$ etex, (0,0)) rotated 180) shifted d2;
draw (thelabel.rt(btex $\mathit{a}$ etex, (0,0)) rotated 180) shifted d3;
draw (thelabel.lft(btex $\mathit{s}$ etex, (0,0)) rotated 180) shifted d4;

% Arrows
drawarrow (d1--d10);
drawarrow reverse(d2..d20);
drawarrow reverse(d3--d30);
drawarrow (d4..d40);

draw (0.4dimXX, -0.1dimYY)
   --(1.6dimXX, -0.1dimYY)
   --(1.6dimXX, 1.1dimYY)
   --(0.4dimXX, 1.1dimYY)
   --cycle;

% Actual wirings

drawarrow (d10..d20) withcolor choiceColor;
drawarrow (d40..d30) withcolor strongColor;

);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
