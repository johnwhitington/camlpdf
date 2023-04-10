CamlPDF
=======

CamlPDF is an OCaml library for reading, writing and modifying PDF files. It is
the basis of the commercial "CPDF" command line tool and
C/C++/Java/Python/.NET/JavaScript API, which is available at
[http://www.coherentpdf.com/](http://www.coherentpdf.com/).

License
---

Copyright Coherent Graphics Ltd 2007 - 2023. Released under the LGPL with
special linking exception. See "LICENSE" for details.

To Build
---

If downloading from Github, obtain the correct source. This means choosing the
tag for a particular version, such as "v2.6". The head of the master branch is
unstable.

1. Run "make". This will build camlpdf.a, camlpdf.cma, camlpdf.cmxa and the
   documentation (in doc/camlpdf/html).

2. If your environment has "ocamlfind", "make install" will install the
   library. Otherwise, use the built outputs as you will.

Alternatively, you can install the latest version with

```
opam install camlpdf
```

Documentation
---

The API documentation, which is built by the makefile in doc/camlpdf/html, can
also be accessed online at
[http://www.coherentpdf.com/camlpdf](http://www.coherentpdf.com/camlpdf).

The file [introduction_to_camlpdf.pdf](introduction_to_camlpdf.pdf) will help
the beginner.

Some level of knowledge of the PDF file format itself, which is large, may be
required. Useful texts are the author's book:

[http://shop.oreilly.com/product/0636920021483.do](http://shop.oreilly.com/product/0636920021483.do)

and the ISO standard for PDF:

[https://www.pdfa-inc.org/product/iso-32000-2-pdf-2-0-bundle-sponsored-access/](https://www.pdfa-inc.org/product/iso-32000-2-pdf-2-0-bundle-sponsored-access/)

Acknowledgments
---

The file [miniz.c](miniz.c) is a (very slightly modified) version of the
miniz.c zlib implementation by Rich Geldreich, available here:

[http://code.google.com/p/miniz/](http://code.google.com/p/miniz/)

The files [flatestubs.c](flatestubs.c), [pdfflate.ml](pdfflate.ml) and
[pdfflate.mli](pdfflate.mli) are a slightly modified version of some parts of
CamlZip by Xavier Leroy. The originals are available here:

[http://pauillac.inria.fr/~xleroy/software.html](http://pauillac.inria.fr/~xleroy/software.html)

The file [rijndael-alg-fst.c](rijndael-alg-fst.c) was written by Vincent
Rijmen, Antoon Bosselaers and Paulo Barreto.

[OCamlMakefile](OCamlMakefile) was written by Markus Mottl. It is available
here:

[http://mmottl.github.io/ocaml-makefile/](http://mmottl.github.io/ocaml-makefile/)
