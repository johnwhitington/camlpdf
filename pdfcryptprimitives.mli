(** Generic encryption primitives for ARC4, AES and the SHA family of digests *)

(** ARC4 encryption (40 bit and 128 bit) given a key and some data *)
val crypt : int array -> Pdfio.bytes -> Pdfio.bytes

(** [aes_decrypt_data nk key data] decrypts AES data for the given key length,
key, and data. If [remove_padding] is [true] (which it is by default), padding
wil be removed from the output *)
val aes_decrypt_data :
  ?remove_padding:bool -> int -> int array -> Pdfio.bytes -> Pdfio.bytes

(** As [aes_decrypt_data] above, but in ECB instead of CBC mode *)
val aes_decrypt_data_ecb :
  ?remove_padding:bool -> int -> int array -> Pdfio.bytes -> Pdfio.bytes

(** [aes_encrypt_data nk key data] encryptes with AES, given a key length, key
and data. The first block (by default a random one) can be overridden by
specifying [firstblock], an array of length 16. *) 
val aes_encrypt_data :
  ?firstblock:int array -> int -> int array -> Pdfio.bytes -> Pdfio.bytes

(** As [aes_encrypt_data], but in ECB instead of CBC mode *)
val aes_encrypt_data_ecb : int -> int array -> Pdfio.bytes -> Pdfio.bytes

(** SHA256 digest *)
val sha256 : Pdfio.input -> string

(** SHA344 digest *)
val sha384 : Pdfio.input -> string

(** SHA512 digest *)
val sha512 : Pdfio.input -> string

(** Types of encryption *)
type encryption = 
  | ARC4 of int * int
  | AESV2
  | AESV3 of bool (** true = iso, false = old algorithm *)

(** Given an object number, generation number, input key and key length in bits,
apply Algorithm 3.1 from the PDF Reference manual to obtain the hash to be used
by the encryption function. *)
val find_hash : encryption -> int32 -> int32 -> int array -> int -> int array

(** Calling [decrypt_stream_data crypt_type encrypt file_encryption_key obj gen key
   keylength r data] decrypts data. *)
val decrypt_stream_data :
  encryption ->
  bool ->
  string option ->
  int -> int -> int array -> int -> int -> Pdfio.bytes -> Pdfio.bytes
