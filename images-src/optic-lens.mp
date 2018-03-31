input opticmacros;

verbatimtex
%&latex
\documentclass{article}
\usepackage{mathpazo}
\usepackage{helvet}
\usepackage{inconsolata}
\begin{document}
etex

beginfig(1);
picture diag;
path p[];
pair u[];
diag:=image(

label.top(btex $\mathit{Lens}$ etex, d0);

% Labels
label.lft(btex $\mathit{s}$ etex, d1);
label.rt(btex $\mathit{a}$ etex, d2);
label.lft(btex $\mathit{t}$ etex, d3);
label.rt(btex $\mathit{b}$ etex, d4);

% Arrows
drawarrow d1--d10;
drawarrow reverse(d2..d20);
drawarrow reverse(d3--d30);
drawarrow (d4..d40);

% Box
draw (0.4dimXX, -0.1dimYY)
   --(1.6dimXX, -0.1dimYY)
   --(1.6dimXX, 1.1dimYY)
   --(0.4dimXX, 1.1dimYY)
   --cycle;

% Actual wirings
drawarrow d10..d20 withcolor strongColor;
draw d12{right}..d34{left} withcolor choiceColor;
drawarrow d40..d30 withcolor choiceColor;

);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
