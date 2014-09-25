val crypt : int array -> Pdfio.bytes -> Pdfio.bytes

val aes_decrypt_data :
  ?remove_padding:bool -> int -> int array -> Pdfio.bytes -> Pdfio.bytes

val aes_decrypt_data_ecb :
  ?remove_padding:bool -> int -> int array -> Pdfio.bytes -> Pdfio.bytes

val aes_encrypt_data :
  ?firstblock:int array -> int -> int array -> Pdfio.bytes -> Pdfio.bytes

val aes_encrypt_data_ecb : int -> int array -> Pdfio.bytes -> Pdfio.bytes

val sha256 : Pdfio.input -> string

val sha384 : Pdfio.input -> string

val sha512 : Pdfio.input -> string

