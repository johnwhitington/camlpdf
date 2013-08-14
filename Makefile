# Build the camlpdf library as byte code and native code
PDFMODS = pdfutil pdfio pdftransform pdfunits pdfpaper pdf pdfcrypt pdfflate \
pdfcodec pdfwrite pdfgenlex pdfread pdfjpeg pdfops pdfdest pdfmarks \
pdfpagelabels pdfpage pdfannot pdffun pdfspace pdfimage pdfafm pdfafmdata \
pdfglyphlist pdftext pdfstandard14 pdfgraphics pdfshapes pdfdate pdfocg \
pdfcff pdftype1 pdftruetype pdftype0 pdfmerge

SOURCES = flatestubs.c $(foreach x,$(PDFMODS),$(x).ml $(x).mli)

PACKS = bigarray

RESULT = camlpdf

LIBINSTALL_FILES = camlpdf.a camlpdf.cma camlpdf.cmxa libcamlpdf_stubs.a \
dllcamlpdf_stubs.* \
$(foreach x,$(PDFMODS),$x.mli) $(foreach x,$(PDFMODS),$x.cmi)

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code-library byte-code-library top htdoc

install : libinstall

-include OCamlMakefile

