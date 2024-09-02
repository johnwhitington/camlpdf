(** Representing PDF Files in Memory *)

(** {2 PDF Objects} *)

type toget

(** A stream is either in memory, or at a position and of a length in an
[Pdfio.input]. *)
type stream =
  | Got of Pdfio.bytes
  | ToGet of toget

(** PDF objects. An object is a tree-like structure containing various things.
A PDF file is basically a directed graph of objects. *)
type pdfobject =
  | Null
  | Boolean of bool
  | Integer of int
  | Real of float
  | String of string
  | Name of string
  | Array of pdfobject list
  | Dictionary of (string * pdfobject) list
  | Stream of (pdfobject * stream) ref
  | Indirect of int

(** {2 The Object map} *)

(** You should not expect to manipulate these types and functions directly. *)

(** This type represents a possibly-parsed, possibly-decrypted, possibly-read-from-an-object-stream object. *)
type objectdata =
  (* Not from an object stream, fully parsed, not necessarily decrypted yet *)
  | Parsed of pdfobject
  (* Was from an object stream, decrypted already when object stream read *)
  | ParsedAlreadyDecrypted of pdfobject
  (* Not parsed yet. Needs to be read from an object, which may still be encrypted *)
  | ToParse
  (* (stream object number, index in stream) Not parsed yet. Will come from an object stream. *)
  | ToParseFromObjectStream of (int, int list) Hashtbl.t * int * int * (int -> int list -> (int * (objectdata ref * int)) list)

type pdfobjmap_key = int

type pdfobjmap = (pdfobjmap_key, objectdata ref * int) Hashtbl.t
(** The object map maps object numbers [pdfobjmap_key] to a reference to the
object data and the generation number *)

(** Make an empty object map *)
val pdfobjmap_empty : unit -> pdfobjmap

(** Find an object in the object map *)
val pdfobjmap_find : pdfobjmap_key -> pdfobjmap -> objectdata ref * int

(** The objects. Again, you won't normally manipulate this directly.
[maxobjnum] is the biggest object number seen yet. [parse] is a function to
parse a non-object stream object given its object number, [pdfobjects] is the
object map itself. [object_stream_ids] is a hash table of (object number,
was-stored-in-obect-stream-number) pairs, which is used to reconstruct stream
objects when preserving them upon write. *)
type pdfobjects =
  {mutable maxobjnum : int;
   mutable parse : (pdfobjmap_key -> pdfobject) option;
   mutable pdfobjects : pdfobjmap;
   mutable object_stream_ids : (int, int) Hashtbl.t}

(** {2 The PDF document} *)

type saved_encryption =
  {from_get_encryption_values :
     Pdfcryptprimitives.encryption * string * string * int32 * string * string option * string option;
   encrypt_metadata : bool;
   perms : string}

type deferred_encryption =
  {crypt_type : Pdfcryptprimitives.encryption;
   file_encryption_key : string option;
   obj : int;
   gen : int;
   key : int array;
   keylength : int;
   r : int}

(** A Pdf document. Major and minor version numbers, object number of root, the
objects objects and the trailer dictionary as a [Dictionary] [pdfobject]. *)
type t =
  {mutable major : int;
   mutable minor : int;
   mutable root : int;
   mutable objects : pdfobjects;
   mutable trailerdict : pdfobject;
   mutable was_linearized : bool;
   mutable saved_encryption : saved_encryption option}

(** The empty document (PDF 1.0, no objects, no root, empty trailer dictionary).
Note this is not a well-formed PDF. *)
val empty : unit -> t

(** {2 Exceptions and errors} *)

(** This exception is raised when some malformity in a PDF is found -- quite a
wide range of circumstances, and may be raised from many functions. *)
exception PDFError of string

(** This function, given a [Pdfio.input] and an ancilliary string, builds an
error string which includes the source of the Pdfio.input (filename, string,
bytes etc) so we can trace what it was originally built from *)
val input_pdferror : Pdfio.input -> string -> string

(** {2 Useful utilities} *)

(** Get a stream from disc if it hasn't already been got. The input is a
[Stream pdfobject]. *)
val getstream : pdfobject -> unit

(** Return a float from a [Real], an [Int] or an [Indirect] *)
val getnum : t -> pdfobject -> float

(** Lookup an object in a document, parsing it if required. Raises [Not_found]
if the object does not exist. *)
val lookup_obj : t -> int -> pdfobject

(** [lookup_fail errtext doc key dict] looks up a key in a PDF dictionary or the
dictionary of a PDF stream. Fails with [PDFError errtext] if the key is not
found. Follows indirect object links. *)
val lookup_fail : string -> (t -> string -> pdfobject -> pdfobject)

(** Same, but with customised exception. *)
val lookup_exception : exn -> t -> string -> pdfobject -> pdfobject

(** [lookup_direct doc key dict] looks up the key, resolving indirections at
    source and destination, returning an option type. *) 
val lookup_direct : t -> string -> pdfobject -> pdfobject option

(** [lookup_immediate key dict] looks up the key returning the value, without
following indirects at either source or destination. *)
val lookup_immediate : string -> pdfobject -> pdfobject option

(** [lookup_chain doc start keys] looks up the key in a nested dictionary. For
    example [lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot";
    "/RoleMap"]] *)
val lookup_chain : t -> pdfobject -> string list -> pdfobject option

(** Return the object number of an indirect dictionary object, if it is indirect. *)
val indirect_number : t -> string -> pdfobject -> int option

(** Same as [lookup_direct], but allow a second, alternative key. *)
val lookup_direct_orelse :
  t -> string -> string -> pdfobject -> pdfobject option

(** Remove a dictionary entry, if it exists. *)
val remove_dict_entry : pdfobject -> string -> pdfobject

(** [replace_dict_entry dict key value] replaces a dictionary entry, raising [Not_found] if it's not there. *)
val replace_dict_entry : pdfobject -> string -> pdfobject -> pdfobject

(** [add_dict_entry dict key value] adds a dictionary entry, replacing if already there. *)
val add_dict_entry : pdfobject -> string -> pdfobject -> pdfobject

(** Make a PDF object direct -- that is, follow any indirect links. *)
val direct : t -> pdfobject -> pdfobject

(** Return the size of the object map. *)
val objcard : t -> int

(** Remove the given object *)
val removeobj : t -> int -> unit

(** Add an object. Returns the number chosen. *)
val addobj : t -> pdfobject -> int

(** Same as [addobj], but pick a number ourselves. *)
val addobj_given_num : t -> (int * pdfobject) -> unit

(** {2 Compound structures} *)

(** Parse a PDF rectangle structure into min x, min y, max x, max y. *) 
val parse_rectangle : t -> pdfobject -> float * float * float * float

(** Calling [parse_matrix pdf name dict] parses a PDF matrix found under
key [name] in dictionary [dict] into a [Transform.transform_matrix]. If there is
no matrix, the identity matrix is returned. *)
val parse_matrix : t -> string -> pdfobject -> Pdftransform.transform_matrix 

(** Build a matrix [pdfobject]. *)
val make_matrix : Pdftransform.transform_matrix -> pdfobject

(** Make a number of PDF documents contain no mutual object numbers. They can
then be merged etc. without clashes. *)
val renumber_pdfs : t list -> t list

(** Given a dictionary and a prefix (e.g gs), return a name, starting with the
prefix, which is not already in the dictionary (e.g /gs0). *)
val unique_key : string -> pdfobject -> string

(** {2 Iteration} *)

(** Iterate over the objects in a document. The iterating functions recieves both
object number and object from the object map. *)
val objiter : (int -> pdfobject -> unit) -> t -> unit

(** The same, but in object number order. *)
val objiter_inorder : (int -> pdfobject -> unit) -> t -> unit

(** Iterate over the objects in a document. The iterating functions recieves
object number, generation number and object from the object map. *)
val objiter_gen : (int -> int -> pdfobject -> unit) -> t -> unit

(** Map over all pdf objects in a document. Does not include trailer dictionary. *)
val objselfmap : (pdfobject -> pdfobject) -> t -> unit

(** Iterate over just the stream objects in a document. *)
val iter_stream : (pdfobject -> unit) -> t -> unit

(** {2 Garbage collection} *)

(** Garbage-collect a pdf document. *)
val remove_unreferenced : t -> unit

(** {2 Miscellaneous} *)

(** These functions were previsouly undocumented. They are documented here for
    now, and in the future will be categorised more sensibly. *)

(** True if a character is PDF whitespace. *)
val is_whitespace : char -> bool

(** True if a character is not PDF whitespace. *)
val is_not_whitespace : char -> bool

(** True if a character is a PDF delimiter. *)
val is_delimiter : char -> bool

(** List, in order, the page reference numbers of a PDF's page tree. *)
val page_reference_numbers : t -> int list

(** List the object numbers in a PDF. *)
val objnumbers : t -> int list

(** Use the given function on each element of a PDF dictionary. *)
val recurse_dict :
  (pdfobject -> pdfobject) -> (string * pdfobject) list -> pdfobject 

(** Similarly for an [Array]. The function is applied to each element. *)
val recurse_array :
  (pdfobject -> pdfobject) -> pdfobject list -> pdfobject 

(** Calculate the changes required to renumber a PDF's objects 1..n. *)
val changes : t -> (int, int) Hashtbl.t

(** Perform the given renumberings on a PDF. *)
val renumber : (int, int) Hashtbl.t -> t -> t

(** Renumber an object given a change table. *)
val renumber_object_parsed : t -> (int, int) Hashtbl.t -> pdfobject -> pdfobject

(** Fetch a stream, if necessary, and return its contents (with no processing). *)
val bigarray_of_stream : pdfobject -> Pdfio.bytes

(** Make a objects entry from a parser and a list of (number, object) pairs. *)
val objects_of_list :
  (int -> pdfobject) option -> (int * (objectdata ref * int)) list -> pdfobjects

(** Calling [objects_referenced no_follow_entries no_follow_contains pdf
    pdfobject] find the objects reachable from the given object. Dictionary
    keys in [no_follow_entries] are not explored. Dictionaries containing
    entries in [no_follow_contains] are not explored. *)
val objects_referenced : string list -> (string * pdfobject) list -> t -> pdfobject -> int list

(** Generate and ID for a PDF document given its prospective file name (and
    using the current date and time). If the file name is blank, the ID is
    still likely to be unique, being based on date and time only. If
    environment variable CAMLPDF_REPRODUCIBLE_IDS=true is set, the ID will instead
    be set to a standard value. *)
val generate_id : t -> string -> (unit -> float) -> pdfobject

(** Return the document catalog. *)
val catalog_of_pdf : t -> pdfobject

(** Find the indirect reference given by the value associated with a key in a
dictionary. *)
val find_indirect : string -> pdfobject -> int option

(** Calling [nametree_lookup pdf k dict] looks up the name in the document's
    name tree *)
val nametree_lookup : t -> pdfobject -> pdfobject -> pdfobject option

(** Return an ordered list of the key-value pairs in a given name tree. *)
val contents_of_nametree : t -> pdfobject -> (pdfobject * pdfobject) list

(** Copy a PDF data structure so that nothing is shared with the original. *)
val deep_copy : t -> t

(** Change the /ID string in a PDF's trailer dicfionary *)
val change_id : t -> string -> unit

(**/**)

val replace_chain : t -> string list -> string * pdfobject -> unit

(* This is only for the use of Pdfread for when the /Length is incorrect. *)
type toget_crypt =
  | NoChange
  | ToDecrypt of deferred_encryption

val length_of_toget : toget -> int
val input_of_toget : toget -> Pdfio.input
val position_of_toget : toget -> int
val toget : ?crypt:toget_crypt -> Pdfio.input -> int -> int -> toget

(* For inter-module recursion within CamlPDF, hence undocumented. *)
val string_of_pdf : (pdfobject -> string) ref

val transform_rect : t -> Pdftransform.transform_matrix -> pdfobject -> pdfobject

val transform_quadpoints : t -> Pdftransform.transform_matrix -> pdfobject -> pdfobject
