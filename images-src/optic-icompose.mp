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
pair off;
diag:=image(


% Labels
label.lft(btex $\mathit{s}$ etex, d1);
label.rt(btex $\mathit{i}$ etex, (xpart d2,ypart d21));
label.rt(btex $\mathit{u}$ etex, (xpart d2,ypart d22));
label.lft(btex $\mathit{t}$ etex, d3);
label.rt(btex $\mathit{v}$ etex, d4);

% Arrows
drawarrow d1--d10;
drawarrow reverse((xpart d2,ypart d21)..d21);
drawarrow reverse((xpart d2,ypart d22)..d22);
drawarrow reverse(d3--d30);
drawarrow (d4..d40);

% Box
draw (0.4dimXX, -0.1dimYY)
   --(1.6dimXX, -0.1dimYY)
   --(1.6dimXX, 1.1dimYY)
   --(0.4dimXX, 1.1dimYY)
   --cycle;

% Actual wirings
draw d10--d14 withcolor strongColor;
drawarrow d14{right}..d22{right} withcolor strongColor;
drawarrow d14{right}..d21{right} withcolor strongColor;
draw d12{right}..d34{left} withcolor choiceColor;
drawarrow d40..d30 withcolor choiceColor;

o = xpart(urcorner(thelabel.rt(btex $\mathit{u}$ etex, (xpart d2,ypart d22)))) + 0.05dimXX;
off = (o, 0);

% labels
label.rt(btex $\mathit{i}$ etex, (xpart d2 + o,0.0dimYY));
label.rt(btex $\mathit{j}$ etex, (xpart d2 + o,0.2dimYY));
label.rt(btex $\mathit{a}$ etex, (xpart d2 + o,0.4dimYY));
label.rt(btex $\mathit{b}$ etex, d4 shifted off);

% Arrows
drawarrow ((xpart d1, ypart d11) shifted off .. d11 shifted off);
drawarrow ((xpart d1, ypart d12) shifted off .. d12 shifted off);
drawarrow reverse((xpart d2 + o,0.0dimYY)..(xpart d22 + o, 0.0dimYY));
drawarrow reverse((xpart d2 + o,0.2dimYY)..(xpart d22 + o, 0.2dimYY));
drawarrow reverse((xpart d2 + o,0.4dimYY)..(xpart d22 + o, 0.4dimYY));
drawarrow reverse(d3 shifted off -- d30 shifted off);
drawarrow (d4 shifted off ..d40 shifted off);

% Box
draw (0.4dimXX + o, -0.1dimYY)
   --(1.6dimXX + o, -0.1dimYY)
   --(1.6dimXX + o, 1.1dimYY)
   --(0.4dimXX + o, 1.1dimYY)
   --cycle;

% Actual wirings
drawarrow ((xpart d10 + o, 0.1dimYY){right}..(xpart d22 + o, 0.0dimYY){right}) dashed dashpattern(on 2bp off 3bp) withcolor strongColor;

draw (xpart d10 + o, 0.3dimYY)--(xpart d14 + o, 0.3dimYY) withcolor strongColor;
drawarrow (xpart d14 + o, 0.3dimYY){right}..(xpart d22 + o, 0.2dimYY){right} withcolor strongColor;
drawarrow (xpart d14 + o, 0.3dimYY){right}..(xpart d22 + o, 0.4dimYY){right} withcolor strongColor;

draw (d13 shifted off){right}..(d34 shifted off){left} withcolor choiceColor;
drawarrow (d40 shifted off)..(d30 shifted off) withcolor choiceColor;

% i arrow drawarrow reverse((xpart d2,ypart d21)..d21);

label.top(btex $\mathit{icompose}$ etex, (o - 0.2dimXX, ypart d0));
);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
