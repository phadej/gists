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

label.top(btex $\mathit{Setter}$ etex, d0);

% Labels
label.lft(btex $\mathit{s}$ etex, d1);
label.rt(btex $\mathit{a}$ etex, d2);
label.lft(btex $\mathit{t}$ etex, d3);
label.rt(btex $\mathit{b}$ etex, d4);

% Arrows
drawarrow d1--d10;
drawarrow reverse(d3--d30);
drawarrow reverse(d2..d20);
drawarrow (d4..d40);

% Box
draw (0.4dimXX, -0.1dimYY)
   --(1.6dimXX, -0.1dimYY)
   --(1.6dimXX, 1.1dimYY)
   --(0.4dimXX, 1.1dimYY)
   --cycle;

d141 = d14 shifted (0.1dimXX,0);
d341 = d34 shifted (0.1dimXX,0);

d142 = d14 shifted (0.2dimXX,0);
d342 = d34 shifted (0.2dimXX,0);

% Actual wirings
draw d14{right}..d34{left} withcolor affineColor;
draw d141{right}..d341{left} withcolor affineColor;
draw d142{right}..d342{left} withcolor affineColor;

draw d10..d142 withcolor affineColor;
drawarrow d342..d30 withcolor affineColor;

draw d142..d20 dashed dashpattern(on 2bp off 3bp) withcolor strongColor;
draw d342..d40 dashed dashpattern(on 2bp off 3bp) withcolor choiceColor;

);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
