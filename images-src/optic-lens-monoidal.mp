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

label.top(btex $\mathit{Lens}$ etex, d0);
label.top(btex $\mathit{Monoidal}$ etex, d0 shifted off);

% Labels
label.lft(btex $\mathit{[a]}$ etex, d1);
label.rt(btex $\mathit{?}$ etex, d2);
label.lft(btex $\mathit{[b]}$ etex, d3);
label.rt(btex $\mathit{?}$ etex, d4);

label.rt(btex $\mathit{a^\star}$ etex, d2 shifted off);
label.rt(btex $\mathit{b^\star}$ etex, d4 shifted off);

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

p1 = d12{right}..d34{left};
draw p1 withcolor choiceColor;
label.urt(btex $\star$ etex, point 0.5 of p1);

drawarrow d10 shifted off ..d20 shifted off withcolor strongColor;
drawarrow (d14 shifted off){right}..(d21 shifted off){right} withcolor strongColor;
drawarrow (d14 shifted off){right}..(d22 shifted off){right} withcolor strongColor;

drawarrow d40 shifted off ..d30 shifted off withcolor choiceColor;
draw (d41 shifted off){left}..(d34 shifted off){left} withcolor choiceColor;
draw (d42 shifted off){left}..(d34 shifted off){left} withcolor choiceColor;
);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
