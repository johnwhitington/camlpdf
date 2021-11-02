# Build the camlpdf library as byte code and, if available, native code
PDFMODS = pdfutil pdfio pdftransform pdfunits pdfpaper pdfcryptprimitives \
  pdf pdfcrypt pdfflate pdfcodec pdfwrite pdfgenlex pdfread pdfjpeg pdfops \
  pdfdest pdfmarks pdfpagelabels pdfpage pdfannot pdffun pdfspace pdfimage \
  pdfafm pdfafmdata pdfglyphlist pdftext pdfstandard14 pdfgraphics pdfshapes \
  pdfdate pdfocg pdfcff pdftype1 pdftruetype pdftype0 pdfmerge

SOURCES = flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c \
	  $(foreach x,$(PDFMODS),$(x).ml $(x).mli)

RESULT = camlpdf

OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string -w -3
OCAMLBCFLAGS = -g -safe-string -w -3
OCAMLLDFLAGS = -g

TARGETS := byte-code-library htdoc

LIBINSTALL_FILES = camlpdf.cma libcamlpdf_stubs.a \
  dllcamlpdf_stubs.* $(foreach x,$(PDFMODS),$x.mli) \
  $(foreach x,$(PDFMODS),$x.cmt) $(foreach x,$(PDFMODS),$x.cmi)

ifneq ($(shell which ocamlopt),)
  TARGETS += native-code-library
  LIBINSTALL_FILES += camlpdf.a camlpdf.cmxa $(foreach x,$(PDFMODS),$x.cmx)
endif

all : $(TARGETS)

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf *.cmt *.cmti *.zlib

install : libinstall

-include OCamlMakefile
