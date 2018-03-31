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
numeric o;
numeric t[];
pair off;
diag:=image(

o = 2.3dimXX;
off = (o, 0);

label.top(btex $\mathit{Prism}$ etex, d0);
label.top(btex $\mathit{Lens}$ etex, d0 shifted off);

% Labels
label.lft(btex $\mathit{s}$ etex, d1);
label.rt(btex $\mathit{u}$ etex, d2);
label.lft(btex $\mathit{t}$ etex, d3);
label.rt(btex $\mathit{v}$ etex, d4);

label.rt(btex $\mathit{a}$ etex, d2 shifted off);
label.rt(btex $\mathit{b}$ etex, d4 shifted off);

% Arrows
drawarrow d1--d10;
drawarrow reverse(d2..d20);
drawarrow reverse(d3--d30);
drawarrow (d4..d40);

drawarrow d1 shifted off--d10 shifted off;
drawarrow reverse(d2 shifted off..d20 shifted off);
drawarrow reverse(d3 shifted off--d30 shifted off);
drawarrow (d4 shifted off..d40 shifted off);

% Box
draw (0.4dimXX, -0.1dimYY)
   --(1.6dimXX, -0.1dimYY)
   --(1.6dimXX, 1.1dimYY)
   --(0.4dimXX, 1.1dimYY)
   --cycle;

draw (0.4dimXX + o, -0.1dimYY)
   --(1.6dimXX + o, -0.1dimYY)
   --(1.6dimXX + o, 1.1dimYY)
   --(0.4dimXX + o, 1.1dimYY)
   --cycle;

% Actual wirings
drawarrow d10..d20 withcolor strongColor;
drawarrow d40..d30 withcolor choiceColor;

drawarrow d10 shifted off..d20 shifted off withcolor strongColor;
drawarrow d40 shifted off..d30 shifted off withcolor choiceColor;

% draw d10{right}..d34{left} withcolor affineColor;
p0=d12{right}..d22{right};
p1=d14{right}..d31{left};

(whatever,t1) = p1 intersectiontimes p0;

draw subpath(0, t1-0.03) of p0 dashed evenly withcolor choiceColor;
draw subpath(t1+0.03, length p0) of p0 dashed evenly withcolor choiceColor;

draw (d22--d12 shifted off) dashed evenly withcolor choiceColor;

drawarrow p1 withcolor strongColor;

draw (d12 shifted off){right}..(d34 shifted off){left} withcolor choiceColor;

);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
