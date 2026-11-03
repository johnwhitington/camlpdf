# Changes

## 2.9.3 (October 2026)

- Expose Pdfannot.string_of_subtype

## 2.9.2 (September 2026)

- New CAMLPDF_SHOW_EXT environment variable to show external commands

## 2.9.1 (May 2026)

- Support reading of file at given revision
- Support writing with incremental update
- Read (extended) and (new) write OCGs
- Flatness graphics parameter now (correctly) real
- Expand definition of Standard Font for malformed files
- Read widths for vertical mode fonts properly
- Allow supression of identical adjacent logs in Pdfe

## 2.9 (March 2026)

- Add cycle detection in find_pages_quick for malformed files
- CCITT Group 3 encoder (native) and Group 4 encoder (via imagemagick)
- New Pdf.remove_chain
- Check object requires renumbering before doing so (speed)
- Allow extraction of encryption when lack of password prevents reading
- Repair name/number trees with duplicate keys (better)
- Fix bad name detection and improve error message
- Fix regression where bookmark strings were mangled
- Fix (partially) efficiency of page renumbering on very large files
- Disambiguate annotation object numbers where a page is copied
- Allow optional use of jbig2dec to decode JBIG2 streams
- Failure to decrypt an object due to malformity no longer fatal

## 2.8.1 (April 2025)

- Pdf.lookup_chain can look within arrays
- Pdf.replace_chain can replace within arrays
- Repair name/number trees with duplicate keys
- Pdfpage.pagenumber_of_target works on GoTo actions
- Allow optional shallow reading of destinations
- Ensure that shallow destinations are transformed
- Empty name now a warning not an error (written as /)

## 2.8 (December 2024)

- Read and write bookmark colour and flags
- Fixed a bug in Pdfst.renumber_parent_trees
- Only call renumber_parent_trees when processing struct trees

## 2.7.2 (October 2024)

- Process destination name tree in Pdfpage.pdf_of_pages
- Options to change the whitespace between operators
- Allow writing of comments in streams
- Allow addition of top-level Document in struct tree when merging
- New lookup_chain / replace_chain functions in Pdf
- Revert 2.7 "Remember and reapply inline image decode parameters"

## 2.7.1 (July 2024)

- Fix sign extension in js_of_ocaml
- Pdfannot expanded to more annotation types
- Extract code to Pdftree module (API subject to change)
- Structure tree optionally trimmed on Pdf.pdf_of_pages
- Structure tree optionally trimmed & merged in Pdfmerge

## 2.7 (February 2024)

- Add opam file in-source
- Cope with more malformed bookmarks
- Remember and reapply inline image decode parameters
- Pdfimage extracts and stores JBIG2Globals
- Option to compress a stream only if it is made smaller
- Encode predictor for PNG Sub (bpc = 8, 3 components)
- Fix for LZWDecode streams which overfill the table
- New endpage_fast
- Remove some very old unused Pdfimage code
- Reconstruct tree in Pdfpage.pdf_of_pages for better bookmarks
- Cope with predictor dictionary not being end of inline image dictionary

## 2.6 (July 2023)

### Merging improvements

- Keep major PDF version number
- Merge /StructTreeRoot entries (Structure hierarchy / Tagged PDF)
- Disambiguate destination name trees when merging
- Preserve named destinations in bookmarks when merging
- Remove /OpenAction on merge
- Preserve first metadata seen on merge

### Other

- Cope with files with no /MediaBox at all
- Write IDs as Hexadecimal strings
- Replace Stream/Genlex and other deprecations for OCaml 5
- Allows malformed numbers --1, --2.5 etc.
- Support for alternative stubs for js_of_ocaml
- Push mk_id down to pdf_to_output so it works when writing to non-file destinations
- Fix Pdf.getnum and Pdf.parse_rectangle to cope with indirects
- Ignore data check errors in flate decoding
- Now reads many more files from Stressful PDF Corpus
- Revert 'build byte code top level camlpdf.top by default'
- Pdfpage.add_prefix now detects and fixes non-ISO PDFs
- Loosen EI check on inline image lexing
- Compress inline images upon writing if uncompressed
- Retired old unused modules to old/
- Cope with /Crypt identity filters
- Ability to redirect error output
- Harden ASCII85Decode against whitespace

## 2.5 (January 2022)

- Build byte code top level camlpdf.top by default
- Replace deprecated C interface functions for OCaml 5.0
- Document most undocumented functions
- Pdfpage.change_pages now preserves object streams
- Width calculation in Pdfstandard14 now more efficient
- Charcode and text extractors have font not fontdict counterparts
- Pdftext.charcode_extractor_of_font copes with more encodings
- Add Pdftext.simplify_utf16be
- Merge now merges AcroForms
- Fix Pdfio.setinit and friends to deal with 0-length data
- Harden Pdfmarks against erroneous empty /Title in doc outline
- AFM and glyphlists loaded from compressed sources
- Environment variable CAMLPDF_REPRODUCIBLE_IDS for testing
- Effectively make stderr unbuffered for all output
- A dictionary entry with null for its value now does not exist
- A missing mediabox now not fatal - we use the most-recently-seen

## 2.4 (June 2021)

- Prefixed all C stubs to avoid clashes with zlib / cryptokit
- Fix for zero-sized Pdfio.input_outputs
- Bad interaction between deferred decryption and object streams fixed
- Optional content groups merged when merging
- Pdfpage.change_pages can now alter bookmark destinations for transformed pages
- Preserves zero bytes in malformed names
- Merged files get fresh /ID
- Pdfpagelabels.write now removes labels when given an empty list

## 2.3 (patchlevel 2, 2020)

- Bad interaction between deferred decryption and object streams worked around

## 2.3 (patchlevel 1, December 2019)

- Updated Makefile to build on bytecode-only architectures
  (thanks Ralf Treinen)

## 2.3 (October 2019)

- Malformed file reading for files with content before the header now works
- Switches to disable malformed file reading or always read as if malformed
- Fix to preserve integers > 2^30 or < -2^30 on 32 bit systems
- Allow [/DCT] as well as /DCT in inline image filter specifications
- Improvements to text width calculation (thanks Alain Frisch)

## 2.2 (patchlevel 1, September 2017)

- Code for adding annotations (thanks @waclena)
- Indirect encryption dictionaries
- Workaround for Adobe "Error 21" on re-saving encrypted files
- Fix reading of null objects in streams

## 2.2 (2017)

- Keeps was_linearized flag with every loaded PDF

## 2.1 (November 2014)

- Excised linearization. We recommend qpdflib / qpdf for this task now.
- Encryption now performed by fast C routines, replacing the OCaml ones
- Faster parsing of delayed object streams on large files
- New implementation of Pdf.page_reference_numbers. More robust
- Faster parsing by using better primitive operations for I/O
- Tighter spacing of output in Pdfwrite leading to smaller files
- Fixed pdf_of_pages not to produce duplicate page objects when multiple parts of the output pdf come from the same input pdf
- Pdfpagelabels bug fixes, especially to alphabetic labels
- Read StemV etc. values from the AFM header directly
- Object streams may be written uncompressed for manual inspection
- Recrypting overhauled. Now a first class citizen.

## 1.7 (30th August 2013)

- Support for writing with object streams
- AES256ISO encryption support
- More compact writing of files
- Support for reading many malformed files
- Now under a standard LGPL license
- Has no dependencies
- First import into git for use with GitHub for open development
- Support for ocamlfind
