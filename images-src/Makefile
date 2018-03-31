SVGIMAGES=profunctor-optics optics-hierarchy \
	  optic-lens optic-prism \
	  optic-identity optic-iso \
	  optic-prim-review optic-review \
	  optic-prim-getter optic-getter \
          optic-iso-flipped-1 optic-iso-flipped-2 \
          optic-prism-flipped \
          optic-traversal1 optic-traversal \
          optic-affine-fold optic-fold1 optic-fold \
	  optic-setter \
	  optic-indexed optic-icompose \
	  optic-affine-traversal optic-affine-traversal-2 \
	  optic-affine-composition optic-lens-monoidal

PNGIMAGES=lens-hierarchy lens-simple

all : $(PNGIMAGES:%=%.png) $(SVGIMAGES:%=%.svg) $(SVGIMAGES:%=%.png) $(SVGIMAGES:%=%.mps)

%.png : %.dot
	dot -Tpng $< -o $@

MPOST=mpost
SVGPARAMS=-s defaultscale:=8 -s outputformat='"svg"' -s outputtemplate='"%j.svg"'
MPSPARAMS=-s outputformat='"mps"' -s outputtemplate='"%j.mps"'

%.mp : %.hs
	runhaskell $< > $@

%.mps : %.mp
	$(MPOST) $(MPSPARAMS) $<

%.svg : %.mp
	$(MPOST) $(SVGPARAMS) $<

%.png : %.svg
	inkscape --export-png=$@ --export-dpi=150 --export-background-opacity=0 --without-gui $<
