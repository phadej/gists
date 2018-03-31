input macros;

dimXX=3cm;
dimYY=1.2cm;

color strongColor, choiceColor, wanderColor, getterColor, reviewColor, cartoColor;

strongColor := (0, 0, 0.6);
choiceColor := (0.7, 0, 0);
wanderColor := (0, 0.6, 0);
getterColor := (1, 0.5, 0);
reviewColor := (0.6, 0, 1);
cartoColor := (0.5, 0.5, 0.5);

vardef rightArrowhead(expr p) =
  save u, z, n;
  pair u;
  pair z[];
  numeric n;

  n = arctime 1mm of p;
  u = unitvector(direction n of reverse p);
  z0 = point 1 of p;
  z1 = point n of (reverse p);
  z2 = z1 shifted (0.5mm * (u rotated 90));

  z0--z1--z2--cycle
enddef;

vardef leftArrowhead(expr p) =
  save u, z, n;
  pair u;
  pair z[];
  numeric n;

  n = arctime 1mm of p;
  u = unitvector(direction n of reverse p);
  z0 = point 1 of p;
  z1 = point n of (reverse p);
  z2 = z1 shifted (0.5mm * (u rotated -90));

  z0--z1--z2--cycle
enddef;

vardef bothArrowhead(expr p) =
  save u, z, n;
  pair u;
  pair z[];
  numeric n;

  n = arctime 1mm of p;
  u = unitvector(direction n of reverse p);
  z0 = point 1 of p;
  z1 = point n of (reverse p);
  z2 = z1 shifted (0.5mm * (u rotated -90));
  z3 = z1 shifted (0.5mm * (u rotated 90));

  z0--z3--z2--cycle
enddef;

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
picture q[];
path p[];
pair u[];
path w[];
pair h[];
diag:=image(
