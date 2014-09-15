# Build the camlpdf library as byte code and native code
PDFMODS = ../pdfutil ../pdfio ../pdftransform ../pdfunits ../pdfpaper ../pdf \
../pdfcrypt ../pdfflate ../pdfcodec ../pdfwrite ../pdfgenlex ../pdfread ../pdfjpeg ../pdfops ../pdfdest \
../pdfmarks ../pdfpagelabels ../pdfpage ../pdfannot ../pdffun ../pdfspace ../pdfimage ../pdfafm \
../pdfafmdata ../pdfglyphlist ../pdftext ../pdfstandard14 ../pdfgraphics ../pdfshapes ../pdfdate \
../pdfocg ../pdfcff ../pdftype1 ../pdftruetype ../pdftype0 ../pdfmerge

SOURCES = ../flatestubs.c $(foreach x,$(PDFMODS),$(x).ml $(x).mli) pdfutop.ml

THREADS = yes

PACKS = utop,bigarray

RESULT = camlpdf

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : top

clean ::
	rm -rf *.di *.d

OCAMLMAKEFILE = ../OCamlMakefile

include $(OCAMLMAKEFILE)
