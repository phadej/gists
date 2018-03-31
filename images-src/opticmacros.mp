input macros;

dimXX=1.5cm;
dimYY=-1.5cm;

color strongColor, choiceColor, wanderColor, getterColor, reviewColor, cartoColor, affineColor;

strongColor := (0, 0, 0.7);
choiceColor := (0.7, 0, 0);
affineColor := (0.6, 0, 0.6);
wanderColor := (0, 0.6, 0);
getterColor := (1, 0.5, 0);
reviewColor := (0.6, 0, 1);
cartoColor := (0.5, 0.5, 0.5);

% 1 --- 2
% |     |
% 3 --- 4

pair d[];

d0:=(1dimXX,-0.1dimYY);

% Labels
d1:=(0dimXX,0.2dimYY);
d2:=(2dimXX,0.2dimYY);
d3:=(0dimXX,0.8dimYY);
d4:=(2dimXX,0.8dimYY);

% Arrows
d10:=(0.4dimXX, 0.2dimYY);
d11:=(0.4dimXX, 0.1dimYY);
d12:=(0.4dimXX, 0.3dimYY);
d13:=(0.4dimXX, 0.4dimYY);
d14:=(0.6dimXX, 0.2dimYY);

d20:=(1.6dimXX, 0.2dimYY);
d21:=(1.6dimXX, 0.1dimYY);
d22:=(1.6dimXX, 0.3dimYY);
d24:=(1.4dimXX, 0.2dimYY);

d30:=(0.4dimXX, 0.8dimYY);
d31:=(0.4dimXX, 0.7dimYY);
d32:=(0.4dimXX, 0.9dimYY);
d34:=(0.6dimXX, 0.8dimYY);

d40:=(1.6dimXX, 0.8dimYY);
d41:=(1.6dimXX, 0.7dimYY);
d42:=(1.6dimXX, 0.9dimYY);
d44:=(1.4dimXX, 0.8dimYY);
