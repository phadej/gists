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
z0=(1.50dimXX,0.00dimYY);
q0=thelabel(btex $\mathit{Equality}$ etex, z0);
draw q0;

z1=(1.50dimXX,1.00dimYY);
q1=thelabel(btex $\mathit{Iso}$ etex, z1);
draw q1;

z2=(0.75dimXX,2.00dimYY);
q2=thelabel(btex $\mathit{Lens}$ etex, z2);
draw q2;

z3=(2.25dimXX,2.00dimYY);
q3=thelabel(btex $\mathit{Prism}$ etex, z3);
draw q3;

z4=(0.00dimXX,2.00dimYY);
q4=thelabel(btex $\mathit{PrimGetter}$ etex, z4);
draw q4;

z5=(0.00dimXX,3.00dimYY);
q5=thelabel(btex $\mathit{Getter}$ etex, z5);
draw q5;

z6=(3.00dimXX,2.00dimYY);
q6=thelabel(btex $\mathit{PrimReview}$ etex, z6);
draw q6;

z7=(3.00dimXX,3.00dimYY);
q7=thelabel(btex $\mathit{Review}$ etex, z7);
draw q7;

z8=(0.75dimXX,3.00dimYY);
q8=thelabel(btex $\mathit{Traversal1}$ etex, z8);
draw q8;

z9=(1.50dimXX,3.00dimYY);
q9=thelabel(btex $\mathit{AffineTraversal}$ etex, z9);
draw q9;

z10=(0.00dimXX,4.00dimYY);
q10=thelabel(btex $\mathit{Fold1}$ etex, z10);
draw q10;

z11=(0.75dimXX,4.00dimYY);
q11=thelabel(btex $\mathit{AffineFold}$ etex, z11);
draw q11;

z12=(1.50dimXX,4.00dimYY);
q12=thelabel(btex $\mathit{Traversal}$ etex, z12);
draw q12;

z13=(0.75dimXX,5.00dimYY);
q13=thelabel(btex $\mathit{Fold}$ etex, z13);
draw q13;

z14=(1.50dimXX,5.00dimYY);
q14=thelabel(btex $\mathit{Setter}$ etex, z14);
draw q14;

p0=clippath(z0..z1, q0, q1);
u0=unitvector(direction 0 of p0);
filldraw bothArrowhead((p0 shifted (0mm*u0 rotated 90))) withcolor blackColor;
draw ((p0 shifted (0mm*u0 rotated 90))) withcolor blackColor;
p1=clippath(z1..z2, q1, q2);
u1=unitvector(direction 0 of p1);
filldraw bothArrowhead((p1 shifted (0mm*u1 rotated 90))) withcolor strongColor;
draw ((p1 shifted (0mm*u1 rotated 90))) withcolor strongColor;
p2=clippath(z1..z3, q1, q3);
u2=unitvector(direction 0 of p2);
filldraw bothArrowhead((p2 shifted (0mm*u2 rotated 90))) withcolor choiceColor;
draw ((p2 shifted (0mm*u2 rotated 90))) withcolor choiceColor;
p3=clippath(z2..z5, q2, q5);
u3=unitvector(direction 0 of p3);
filldraw bothArrowhead((p3 shifted (0mm*u3 rotated 90))) withcolor strongColor;
draw ((p3 shifted (0mm*u3 rotated 90))) withcolor strongColor;
p4=clippath(z4..z5, q4, q5);
u4=unitvector(direction 0 of p4);
filldraw bothArrowhead((p4 shifted (0mm*u4 rotated 90))) withcolor getterColor;
draw ((p4 shifted (0mm*u4 rotated 90))) withcolor getterColor;
p5=clippath(z2..z8, q2, q8);
u5=unitvector(direction 0 of p5);
filldraw leftArrowhead((p5 shifted (0.25mm*u5 rotated 90))) withcolor strongColor;
draw ((p5 shifted (0.25mm*u5 rotated 90))) withcolor strongColor;
filldraw rightArrowhead((p5 shifted ((-0.25mm)*u5 rotated 90))) withcolor wanderColor;
draw ((p5 shifted ((-0.25mm)*u5 rotated 90))) withcolor wanderColor;
p6=clippath(z2..z9, q2, q9);
u6=unitvector(direction 0 of p6);
filldraw bothArrowhead((p6 shifted (0mm*u6 rotated 90))) withcolor strongColor;
draw ((p6 shifted (0mm*u6 rotated 90))) withcolor strongColor;
p7=clippath(z12..z14, q12, q14);
u7=unitvector(direction 0 of p7);
filldraw leftArrowhead((p7 shifted (0.75mm*u7 rotated 90))) withcolor strongColor;
draw ((p7 shifted (0.75mm*u7 rotated 90))) withcolor strongColor;
draw ((p7 shifted (0.25mm*u7 rotated 90))) withcolor wanderColor;
draw ((p7 shifted ((-0.25mm)*u7 rotated 90))) withcolor choiceColor;
filldraw rightArrowhead((p7 shifted ((-0.75mm)*u7 rotated 90))) withcolor cartoColor;
draw ((p7 shifted ((-0.75mm)*u7 rotated 90))) withcolor cartoColor;
p8=clippath(z12..z13, q12, q13);
u8=unitvector(direction 0 of p8);
filldraw leftArrowhead((p8 shifted (0.5mm*u8 rotated 90))) withcolor strongColor;
draw ((p8 shifted (0.5mm*u8 rotated 90))) withcolor strongColor;
draw ((p8 shifted (0*u8 rotated 90))) withcolor wanderColor;
filldraw rightArrowhead((p8 shifted ((-0.5mm)*u8 rotated 90))) withcolor choiceColor;
draw ((p8 shifted ((-0.5mm)*u8 rotated 90))) withcolor choiceColor;
p9=clippath(z8..z10, q8, q10);
u9=unitvector(direction 0 of p9);
filldraw leftArrowhead((p9 shifted (0.25mm*u9 rotated 90))) withcolor strongColor;
draw ((p9 shifted (0.25mm*u9 rotated 90))) withcolor strongColor;
filldraw rightArrowhead((p9 shifted ((-0.25mm)*u9 rotated 90))) withcolor wanderColor;
draw ((p9 shifted ((-0.25mm)*u9 rotated 90))) withcolor wanderColor;
p10=clippath(z8..z12, q8, q12);
u10=unitvector(direction 0 of p10);
filldraw leftArrowhead((p10 shifted (0.25mm*u10 rotated 90))) withcolor strongColor;
draw ((p10 shifted (0.25mm*u10 rotated 90))) withcolor strongColor;
filldraw rightArrowhead((p10 shifted ((-0.25mm)*u10 rotated 90))) withcolor wanderColor;
draw ((p10 shifted ((-0.25mm)*u10 rotated 90))) withcolor wanderColor;
p11=clippath(z5..z10, q5, q10);
u11=unitvector(direction 0 of p11);
filldraw leftArrowhead((p11 shifted (0.25mm*u11 rotated 90))) withcolor getterColor;
draw ((p11 shifted (0.25mm*u11 rotated 90))) withcolor getterColor;
filldraw rightArrowhead((p11 shifted ((-0.25mm)*u11 rotated 90))) withcolor strongColor;
draw ((p11 shifted ((-0.25mm)*u11 rotated 90))) withcolor strongColor;
p12=clippath(z9..z11, q9, q11);
h2 = p10 intersectionpoint p12;
w2 = fullcircle xscaled (1.2*1.5mm) yscaled 1.2mm shifted h2;
u12=unitvector(direction 0 of p12);
filldraw leftArrowhead((p12 shifted (0.25mm*u12 rotated 90))) withcolor strongColor;
draw subpath (ypart(w2 intersectiontimes (p12 shifted (0.25mm*u12 rotated 90))), length (p12 shifted (0.25mm*u12 rotated 90))) of ((p12 shifted (0.25mm*u12 rotated 90))) withcolor strongColor;
draw subpath (0, ypart(w2 intersectiontimes (subpath (0, ypart(p10 intersectiontimes (p12 shifted (0.25mm*u12 rotated 90)))) of ((p12 shifted (0.25mm*u12 rotated 90)))))) of (subpath (0, ypart(p10 intersectiontimes (p12 shifted (0.25mm*u12 rotated 90)))) of ((p12 shifted (0.25mm*u12 rotated 90)))) withcolor strongColor;
filldraw rightArrowhead((p12 shifted ((-0.25mm)*u12 rotated 90))) withcolor choiceColor;
draw subpath (ypart(w2 intersectiontimes (p12 shifted ((-0.25mm)*u12 rotated 90))), length (p12 shifted ((-0.25mm)*u12 rotated 90))) of ((p12 shifted ((-0.25mm)*u12 rotated 90))) withcolor choiceColor;
draw subpath (0, ypart(w2 intersectiontimes (subpath (0, ypart(p10 intersectiontimes (p12 shifted ((-0.25mm)*u12 rotated 90)))) of ((p12 shifted ((-0.25mm)*u12 rotated 90)))))) of (subpath (0, ypart(p10 intersectiontimes (p12 shifted ((-0.25mm)*u12 rotated 90)))) of ((p12 shifted ((-0.25mm)*u12 rotated 90)))) withcolor choiceColor;
p13=clippath(z11..z13, q11, q13);
u13=unitvector(direction 0 of p13);
filldraw leftArrowhead((p13 shifted (0.5mm*u13 rotated 90))) withcolor getterColor;
draw ((p13 shifted (0.5mm*u13 rotated 90))) withcolor getterColor;
draw ((p13 shifted (0*u13 rotated 90))) withcolor strongColor;
filldraw rightArrowhead((p13 shifted ((-0.5mm)*u13 rotated 90))) withcolor choiceColor;
draw ((p13 shifted ((-0.5mm)*u13 rotated 90))) withcolor choiceColor;
p14=clippath(z10..z13, q10, q13);
u14=unitvector(direction 0 of p14);
filldraw leftArrowhead((p14 shifted (0.5mm*u14 rotated 90))) withcolor getterColor;
draw ((p14 shifted (0.5mm*u14 rotated 90))) withcolor getterColor;
draw ((p14 shifted (0*u14 rotated 90))) withcolor strongColor;
filldraw rightArrowhead((p14 shifted ((-0.5mm)*u14 rotated 90))) withcolor wanderColor;
draw ((p14 shifted ((-0.5mm)*u14 rotated 90))) withcolor wanderColor;
p15=clippath(z5..z11, q5, q11);
h1 = p9 intersectionpoint p15;
w1 = fullcircle xscaled (1.2*1.5mm) yscaled 1.2mm shifted h1;
u15=unitvector(direction 0 of p15);
filldraw leftArrowhead((p15 shifted (0.25mm*u15 rotated 90))) withcolor getterColor;
draw subpath (ypart(w1 intersectiontimes (p15 shifted (0.25mm*u15 rotated 90))), length (p15 shifted (0.25mm*u15 rotated 90))) of ((p15 shifted (0.25mm*u15 rotated 90))) withcolor getterColor;
draw subpath (0, ypart(w1 intersectiontimes (subpath (0, ypart(p9 intersectiontimes (p15 shifted (0.25mm*u15 rotated 90)))) of ((p15 shifted (0.25mm*u15 rotated 90)))))) of (subpath (0, ypart(p9 intersectiontimes (p15 shifted (0.25mm*u15 rotated 90)))) of ((p15 shifted (0.25mm*u15 rotated 90)))) withcolor getterColor;
filldraw rightArrowhead((p15 shifted ((-0.25mm)*u15 rotated 90))) withcolor strongColor;
draw subpath (ypart(w1 intersectiontimes (p15 shifted ((-0.25mm)*u15 rotated 90))), length (p15 shifted ((-0.25mm)*u15 rotated 90))) of ((p15 shifted ((-0.25mm)*u15 rotated 90))) withcolor strongColor;
draw subpath (0, ypart(w1 intersectiontimes (subpath (0, ypart(p9 intersectiontimes (p15 shifted ((-0.25mm)*u15 rotated 90)))) of ((p15 shifted ((-0.25mm)*u15 rotated 90)))))) of (subpath (0, ypart(p9 intersectiontimes (p15 shifted ((-0.25mm)*u15 rotated 90)))) of ((p15 shifted ((-0.25mm)*u15 rotated 90)))) withcolor strongColor;
p16=clippath(z3..z9, q3, q9);
u16=unitvector(direction 0 of p16);
filldraw bothArrowhead((p16 shifted (0mm*u16 rotated 90))) withcolor choiceColor;
draw ((p16 shifted (0mm*u16 rotated 90))) withcolor choiceColor;
p17=clippath(z9..z12, q9, q12);
u17=unitvector(direction 0 of p17);
filldraw leftArrowhead((p17 shifted (0.25mm*u17 rotated 90))) withcolor strongColor;
draw ((p17 shifted (0.25mm*u17 rotated 90))) withcolor strongColor;
filldraw rightArrowhead((p17 shifted ((-0.25mm)*u17 rotated 90))) withcolor choiceColor;
draw ((p17 shifted ((-0.25mm)*u17 rotated 90))) withcolor choiceColor;
p18=clippath(z3..z7, q3, q7);
u18=unitvector(direction 0 of p18);
filldraw bothArrowhead((p18 shifted (0mm*u18 rotated 90))) withcolor choiceColor;
draw ((p18 shifted (0mm*u18 rotated 90))) withcolor choiceColor;
p19=clippath(z6..z7, q6, q7);
u19=unitvector(direction 0 of p19);
filldraw bothArrowhead((p19 shifted (0mm*u19 rotated 90))) withcolor reviewColor;
draw ((p19 shifted (0mm*u19 rotated 90))) withcolor reviewColor;
);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
