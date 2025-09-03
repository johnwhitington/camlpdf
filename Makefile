# Build the camlpdf library as byte code and, if available, native code
PDFMODS = pdfe pdfutil pdfio pdftransform pdfunits pdfpaper \
  pdfcryptprimitives pdf pdfcrypt pdfflate pdfcodec pdfwrite pdfgenlex \
  pdfread pdfjpeg pdfops pdfdest pdfmarks pdfpagelabels pdftree pdfst pdfpage \
  pdfannot pdffun pdfspace pdfimage pdfafm pdfafmdata pdfglyphlist pdfcmap \
  pdftext pdfstandard14 pdfdate pdfocg pdfmerge

SOURCES = flatestubs.c rijndael-alg-fst.c stubs-aes.c sha2.c stubs-sha2.c \
	  $(foreach x,$(PDFMODS),$(x).ml $(x).mli)

RESULT = camlpdf

CFLAGS = -o2 -g
OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string
OCAMLBCFLAGS = -g -safe-string
OCAMLLDFLAGS = -g

TARGETS := byte-code-library htdoc

LIBINSTALL_FILES = \
  camlpdf.cma libcamlpdf_stubs.a dllcamlpdf_stubs.* \
  $(foreach x,$(PDFMODS),$x.mli) $(foreach x,$(PDFMODS),$x.cmt) \
  $(foreach x,$(PDFMODS),$x.cmi) $(foreach x,$(PDFMODS),$x.cmti)

ifneq ($(shell ocamlopt -version),)
  TARGETS += native-code-library
  LIBINSTALL_FILES += camlpdf.a camlpdf.cmxa $(foreach x,$(PDFMODS),$x.cmx)
endif

all : $(TARGETS)

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf *.ps *.cmt *.cmti *.zlib
	rm -rf examples/out.pdf examples/out2.pdf examples/foo examples/foo
	rm -rf examples/hello.pdf
	cd examples; make clean

install : libinstall

-include OCamlMakefile
