(* Pdfcrypt primitives, split out *)
open Pdfutil
open Pdfio

external aes_cook_encrypt_key : string -> string = "caml_aes_cook_encrypt_key"

external aes_cook_decrypt_key : string -> string = "caml_aes_cook_decrypt_key"

external aes_encrypt : string -> string -> int -> string -> int -> unit = "caml_aes_encrypt"

external aes_decrypt : string -> string -> int -> string -> int -> unit = "caml_aes_decrypt"

(* 40bit / 128bit Encryption/Decryption Primitives *)

(* Encryption / Decryption given a key. *)
let ksa s key =
  let keylength = Array.length key in
    for i = 0 to 255 do s.(i) <- i done;
    let j = ref 0 in
      for i = 0 to 255 do
        j := (!j + s.(i) + key.(i mod keylength)) mod 256;
        swap s i !j
      done

let prga s pi pj =
  pi := (!pi + 1) mod 256;
  pj := (!pj + s.(!pi)) mod 256;
  swap s !pi !pj;
  s.((s.(!pi) + s.(!pj)) mod 256)

let crypt key data =
  let s, pi, pj, out = Array.make 256 0, ref 0, ref 0, mkbytes (bytes_size data) in
    ksa s key;
    for x = 0 to bytes_size data - 1 do
      bset out x (bget data x lxor prga s pi pj)
    done;
    out

(* AES Encryption and Decryption Primitives *)

(* The state, an array of four length 4 arrays. state.(row).(column) *)
let st = Array.make 16 0

let get x y = Array.unsafe_get st (x * 4 + y)
let put x y v = Array.unsafe_set st (x * 4 + y) v

(* Finite field addition *)
let ( ++ ) = ( lxor )

let tab_e =
  [|0x01; 0x03; 0x05; 0x0f; 0x11; 0x33; 0x55; 0xff; 0x1a; 0x2e; 0x72; 0x96; 0xa1; 0xf8; 0x13; 0x35; 
    0x5f; 0xe1; 0x38; 0x48; 0xd8; 0x73; 0x95; 0xa4; 0xf7; 0x02; 0x06; 0x0a; 0x1e; 0x22; 0x66; 0xaa;
    0xe5; 0x34; 0x5c; 0xe4; 0x37; 0x59; 0xeb; 0x26; 0x6a; 0xbe; 0xd9; 0x70; 0x90; 0xab; 0xe6; 0x31;
    0x53; 0xf5; 0x04; 0x0c; 0x14; 0x3c; 0x44; 0xcc; 0x4f; 0xd1; 0x68; 0xb8; 0xd3; 0x6e; 0xb2; 0xcd; 
    0x4c; 0xd4; 0x67; 0xa9; 0xe0; 0x3b; 0x4d; 0xd7; 0x62; 0xa6; 0xf1; 0x08; 0x18; 0x28; 0x78; 0x88;
    0x83; 0x9e; 0xb9; 0xd0; 0x6b; 0xbd; 0xdc; 0x7f; 0x81; 0x98; 0xb3; 0xce; 0x49; 0xdb; 0x76; 0x9a;
    0xb5; 0xc4; 0x57; 0xf9; 0x10; 0x30; 0x50; 0xf0; 0x0b; 0x1d; 0x27; 0x69; 0xbb; 0xd6; 0x61; 0xa3;
    0xfe; 0x19; 0x2b; 0x7d; 0x87; 0x92; 0xad; 0xec; 0x2f; 0x71; 0x93; 0xae; 0xe9; 0x20; 0x60; 0xa0;
    0xfb; 0x16; 0x3a; 0x4e; 0xd2; 0x6d; 0xb7; 0xc2; 0x5d; 0xe7; 0x32; 0x56; 0xfa; 0x15; 0x3f; 0x41;
    0xc3; 0x5e; 0xe2; 0x3d; 0x47; 0xc9; 0x40; 0xc0; 0x5b; 0xed; 0x2c; 0x74; 0x9c; 0xbf; 0xda; 0x75;
    0x9f; 0xba; 0xd5; 0x64; 0xac; 0xef; 0x2a; 0x7e; 0x82; 0x9d; 0xbc; 0xdf; 0x7a; 0x8e; 0x89; 0x80;
    0x9b; 0xb6; 0xc1; 0x58; 0xe8; 0x23; 0x65; 0xaf; 0xea; 0x25; 0x6f; 0xb1; 0xc8; 0x43; 0xc5; 0x54;
    0xfc; 0x1f; 0x21; 0x63; 0xa5; 0xf4; 0x07; 0x09; 0x1b; 0x2d; 0x77; 0x99; 0xb0; 0xcb; 0x46; 0xca;
    0x45; 0xcf; 0x4a; 0xde; 0x79; 0x8b; 0x86; 0x91; 0xa8; 0xe3; 0x3e; 0x42; 0xc6; 0x51; 0xf3; 0x0e;
    0x12; 0x36; 0x5a; 0xee; 0x29; 0x7b; 0x8d; 0x8c; 0x8f; 0x8a; 0x85; 0x94; 0xa7; 0xf2; 0x0d; 0x17;
    0x39; 0x4b; 0xdd; 0x7c; 0x84; 0x97; 0xa2; 0xfd; 0x1c; 0x24; 0x6c; 0xb4; 0xc7; 0x52; 0xf6; 0x01|]

let tab_l =
  [|0x00; 0x00; 0x19; 0x01; 0x32; 0x02; 0x1a; 0xc6; 0x4b; 0xc7; 0x1b; 0x68; 0x33; 0xee; 0xdf; 0x03;
    0x64; 0x04; 0xe0; 0x0e; 0x34; 0x8d; 0x81; 0xef; 0x4c; 0x71; 0x08; 0xc8; 0xf8; 0x69; 0x1c; 0xc1;
    0x7d; 0xc2; 0x1d; 0xb5; 0xf9; 0xb9; 0x27; 0x6a; 0x4d; 0xe4; 0xa6; 0x72; 0x9a; 0xc9; 0x09; 0x78; 
    0x65; 0x2f; 0x8a; 0x05; 0x21; 0x0f; 0xe1; 0x24; 0x12; 0xf0; 0x82; 0x45; 0x35; 0x93; 0xda; 0x8e;
    0x96; 0x8f; 0xdb; 0xbd; 0x36; 0xd0; 0xce; 0x94; 0x13; 0x5c; 0xd2; 0xf1; 0x40; 0x46; 0x83; 0x38;
    0x66; 0xdd; 0xfd; 0x30; 0xbf; 0x06; 0x8b; 0x62; 0xb3; 0x25; 0xe2; 0x98; 0x22; 0x88; 0x91; 0x10;
    0x7e; 0x6e; 0x48; 0xc3; 0xa3; 0xb6; 0x1e; 0x42; 0x3a; 0x6b; 0x28; 0x54; 0xfa; 0x85; 0x3d; 0xba;
    0x2b; 0x79; 0x0a; 0x15; 0x9b; 0x9f; 0x5e; 0xca; 0x4e; 0xd4; 0xac; 0xe5; 0xf3; 0x73; 0xa7; 0x57;
    0xaf; 0x58; 0xa8; 0x50; 0xf4; 0xea; 0xd6; 0x74; 0x4f; 0xae; 0xe9; 0xd5; 0xe7; 0xe6; 0xad; 0xe8;
    0x2c; 0xd7; 0x75; 0x7a; 0xeb; 0x16; 0x0b; 0xf5; 0x59; 0xcb; 0x5f; 0xb0; 0x9c; 0xa9; 0x51; 0xa0;
    0x7f; 0x0c; 0xf6; 0x6f; 0x17; 0xc4; 0x49; 0xec; 0xd8; 0x43; 0x1f; 0x2d; 0xa4; 0x76; 0x7b; 0xb7;
    0xcc; 0xbb; 0x3e; 0x5a; 0xfb; 0x60; 0xb1; 0x86; 0x3b; 0x52; 0xa1; 0x6c; 0xaa; 0x55; 0x29; 0x9d;
    0x97; 0xb2; 0x87; 0x90; 0x61; 0xbe; 0xdc; 0xfc; 0xbc; 0x95; 0xcf; 0xcd; 0x37; 0x3f; 0x5b; 0xd1;
    0x53; 0x39; 0x84; 0x3c; 0x41; 0xa2; 0x6d; 0x47; 0x14; 0x2a; 0x9e; 0x5d; 0x56; 0xf2; 0xd3; 0xab;
    0x44; 0x11; 0x92; 0xd9; 0x23; 0x20; 0x2e; 0x89; 0xb4; 0x7c; 0xb8; 0x26; 0x77; 0x99; 0xe3; 0xa5;
    0x67; 0x4a; 0xed; 0xde; 0xc5; 0x31; 0xfe; 0x18; 0x0d; 0x63; 0x8c; 0x80; 0xc0; 0xf7; 0x70; 0x07|]
 
(* Finite field multiplication modulo the irreducible polynomial. *)
let ( ** ) a b =
  if a = 0 || b = 0 then 0 else
    let t = Array.unsafe_get tab_l (a land 0xff) land 0xff + Array.unsafe_get tab_l (b land 0xff) land 0xff in
      let t = if t > 255 then t - 255 else t in
        Array.unsafe_get tab_e (t land 0xff)


let sbox =
[|
0x63; 0x7c; 0x77; 0x7b; 0xf2; 0x6b; 0x6f; 0xc5; 0x30; 0x01; 0x67; 0x2b; 0xfe; 0xd7; 0xab; 0x76;
0xca; 0x82; 0xc9; 0x7d; 0xfa; 0x59; 0x47; 0xf0; 0xad; 0xd4; 0xa2; 0xaf; 0x9c; 0xa4; 0x72; 0xc0;
0xb7; 0xfd; 0x93; 0x26; 0x36; 0x3f; 0xf7; 0xcc; 0x34; 0xa5; 0xe5; 0xf1; 0x71; 0xd8; 0x31; 0x15;
0x04; 0xc7; 0x23; 0xc3; 0x18; 0x96; 0x05; 0x9a; 0x07; 0x12; 0x80; 0xe2; 0xeb; 0x27; 0xb2; 0x75;
0x09; 0x83; 0x2c; 0x1a; 0x1b; 0x6e; 0x5a; 0xa0; 0x52; 0x3b; 0xd6; 0xb3; 0x29; 0xe3; 0x2f; 0x84;
0x53; 0xd1; 0x00; 0xed; 0x20; 0xfc; 0xb1; 0x5b; 0x6a; 0xcb; 0xbe; 0x39; 0x4a; 0x4c; 0x58; 0xcf;
0xd0; 0xef; 0xaa; 0xfb; 0x43; 0x4d; 0x33; 0x85; 0x45; 0xf9; 0x02; 0x7f; 0x50; 0x3c; 0x9f; 0xa8;
0x51; 0xa3; 0x40; 0x8f; 0x92; 0x9d; 0x38; 0xf5; 0xbc; 0xb6; 0xda; 0x21; 0x10; 0xff; 0xf3; 0xd2;
0xcd; 0x0c; 0x13; 0xec; 0x5f; 0x97; 0x44; 0x17; 0xc4; 0xa7; 0x7e; 0x3d; 0x64; 0x5d; 0x19; 0x73;
0x60; 0x81; 0x4f; 0xdc; 0x22; 0x2a; 0x90; 0x88; 0x46; 0xee; 0xb8; 0x14; 0xde; 0x5e; 0x0b; 0xdb;
0xe0; 0x32; 0x3a; 0x0a; 0x49; 0x06; 0x24; 0x5c; 0xc2; 0xd3; 0xac; 0x62; 0x91; 0x95; 0xe4; 0x79;
0xe7; 0xc8; 0x37; 0x6d; 0x8d; 0xd5; 0x4e; 0xa9; 0x6c; 0x56; 0xf4; 0xea; 0x65; 0x7a; 0xae; 0x08;
0xba; 0x78; 0x25; 0x2e; 0x1c; 0xa6; 0xb4; 0xc6; 0xe8; 0xdd; 0x74; 0x1f; 0x4b; 0xbd; 0x8b; 0x8a;
0x70; 0x3e; 0xb5; 0x66; 0x48; 0x03; 0xf6; 0x0e; 0x61; 0x35; 0x57; 0xb9; 0x86; 0xc1; 0x1d; 0x9e;
0xe1; 0xf8; 0x98; 0x11; 0x69; 0xd9; 0x8e; 0x94; 0x9b; 0x1e; 0x87; 0xe9; 0xce; 0x55; 0x28; 0xdf;
0x8c; 0xa1; 0x89; 0x0d; 0xbf; 0xe6; 0x42; 0x68; 0x41; 0x99; 0x2d; 0x0f; 0xb0; 0x54; 0xbb; 0x16
|]

let inv_sbox =
[|
0x52; 0x09; 0x6a; 0xd5; 0x30; 0x36; 0xa5; 0x38; 0xbf; 0x40; 0xa3; 0x9e; 0x81; 0xf3; 0xd7; 0xfb;
0x7c; 0xe3; 0x39; 0x82; 0x9b; 0x2f; 0xff; 0x87; 0x34; 0x8e; 0x43; 0x44; 0xc4; 0xde; 0xe9; 0xcb;
0x54; 0x7b; 0x94; 0x32; 0xa6; 0xc2; 0x23; 0x3d; 0xee; 0x4c; 0x95; 0x0b; 0x42; 0xfa; 0xc3; 0x4e;
0x08; 0x2e; 0xa1; 0x66; 0x28; 0xd9; 0x24; 0xb2; 0x76; 0x5b; 0xa2; 0x49; 0x6d; 0x8b; 0xd1; 0x25;
0x72; 0xf8; 0xf6; 0x64; 0x86; 0x68; 0x98; 0x16; 0xd4; 0xa4; 0x5c; 0xcc; 0x5d; 0x65; 0xb6; 0x92;
0x6c; 0x70; 0x48; 0x50; 0xfd; 0xed; 0xb9; 0xda; 0x5e; 0x15; 0x46; 0x57; 0xa7; 0x8d; 0x9d; 0x84;
0x90; 0xd8; 0xab; 0x00; 0x8c; 0xbc; 0xd3; 0x0a; 0xf7; 0xe4; 0x58; 0x05; 0xb8; 0xb3; 0x45; 0x06;
0xd0; 0x2c; 0x1e; 0x8f; 0xca; 0x3f; 0x0f; 0x02; 0xc1; 0xaf; 0xbd; 0x03; 0x01; 0x13; 0x8a; 0x6b;
0x3a; 0x91; 0x11; 0x41; 0x4f; 0x67; 0xdc; 0xea; 0x97; 0xf2; 0xcf; 0xce; 0xf0; 0xb4; 0xe6; 0x73;
0x96; 0xac; 0x74; 0x22; 0xe7; 0xad; 0x35; 0x85; 0xe2; 0xf9; 0x37; 0xe8; 0x1c; 0x75; 0xdf; 0x6e;
0x47; 0xf1; 0x1a; 0x71; 0x1d; 0x29; 0xc5; 0x89; 0x6f; 0xb7; 0x62; 0x0e; 0xaa; 0x18; 0xbe; 0x1b;
0xfc; 0x56; 0x3e; 0x4b; 0xc6; 0xd2; 0x79; 0x20; 0x9a; 0xdb; 0xc0; 0xfe; 0x78; 0xcd; 0x5a; 0xf4;
0x1f; 0xdd; 0xa8; 0x33; 0x88; 0x07; 0xc7; 0x31; 0xb1; 0x12; 0x10; 0x59; 0x27; 0x80; 0xec; 0x5f;
0x60; 0x51; 0x7f; 0xa9; 0x19; 0xb5; 0x4a; 0x0d; 0x2d; 0xe5; 0x7a; 0x9f; 0x93; 0xc9; 0x9c; 0xef;
0xa0; 0xe0; 0x3b; 0x4d; 0xae; 0x2a; 0xf5; 0xb0; 0xc8; 0xeb; 0xbb; 0x3c; 0x83; 0x53; 0x99; 0x61;
0x17; 0x2b; 0x04; 0x7e; 0xba; 0x77; 0xd6; 0x26; 0xe1; 0x69; 0x14; 0x63; 0x55; 0x21; 0x0c; 0x7d
|]

let subbyte b =
  Array.unsafe_get sbox b

let sub_bytes () =
  for r = 0 to 3 do
    for c = 0 to 3 do
      put r c (Array.unsafe_get sbox (get r c))
    done
  done

let inv_sub_bytes () =
  for r = 0 to 3 do
    for c = 0 to 3 do
      put r c (Array.unsafe_get inv_sbox (get r c))
    done
  done

(* Key schedule *)
let keys =
  Array.init 60 (function _ -> String.make 4 '\000')

let word_of_bytes a b c d =
  let s = String.create 4 in
    String.unsafe_set s 0 (Char.unsafe_chr a);
    String.unsafe_set s 1 (Char.unsafe_chr b);
    String.unsafe_set s 2 (Char.unsafe_chr c);
    String.unsafe_set s 3 (Char.unsafe_chr d);
    s

let byte1_of_word s =
  int_of_char (String.unsafe_get s 0)

let byte2_of_word s =
  int_of_char (String.unsafe_get s 1)

let byte3_of_word s =
  int_of_char (String.unsafe_get s 2)

let byte4_of_word s =
  int_of_char (String.unsafe_get s 3)

let subword w =
  word_of_bytes
    (subbyte (byte1_of_word w))
    (subbyte (byte2_of_word w))
    (subbyte (byte3_of_word w))
    (subbyte (byte4_of_word w))

let rotword w =
  word_of_bytes (byte2_of_word w) (byte3_of_word w) (byte4_of_word w) (byte1_of_word w)

let crypt_lxor32 a b =
  let s = String.create 4 in
    s.[0] <- Char.unsafe_chr (int_of_char a.[0] lxor int_of_char b.[0]);
    s.[1] <- Char.unsafe_chr (int_of_char a.[1] lxor int_of_char b.[1]);
    s.[2] <- Char.unsafe_chr (int_of_char a.[2] lxor int_of_char b.[2]);
    s.[3] <- Char.unsafe_chr (int_of_char a.[3] lxor int_of_char b.[3]);
    s
  
let rcon =
 [| word_of_bytes 0x00 0x00 0x00 0x00;
    word_of_bytes 0x01 0x00 0x00 0x00;
    word_of_bytes 0x02 0x00 0x00 0x00;
    word_of_bytes 0x04 0x00 0x00 0x00;
    word_of_bytes 0x08 0x00 0x00 0x00;
    word_of_bytes 0x10 0x00 0x00 0x00;
    word_of_bytes 0x20 0x00 0x00 0x00;
    word_of_bytes 0x40 0x00 0x00 0x00;
    word_of_bytes 0x80 0x00 0x00 0x00;
    word_of_bytes 0x1b 0x00 0x00 0x00;
    word_of_bytes 0x36 0x00 0x00 0x00;
    word_of_bytes 0x6c 0x00 0x00 0x00;
    word_of_bytes 0xd8 0x00 0x00 0x00;
    word_of_bytes 0xab 0x00 0x00 0x00;
    word_of_bytes 0x4d 0x00 0x00 0x00;
    word_of_bytes 0x9a 0x00 0x00 0x00 |]

(* Key expansion *)
let key_expansion nk key = aes_cook_encrypt_key (string_of_int_array key)
  (*try
    let nr = nk + 6 in
    let temp = ref (String.create 4)
    in let i = ref 0 in
      while (!i < nk) do
        keys.(!i) <-
          word_of_bytes
            key.(4 * !i) key.(4 * !i + 1) key.(4 * !i + 2) key.(4 * !i + 3);
        incr i
      done;
      i := nk;
      while (!i < 4 * (nr + 1)) do
        temp := keys.(!i - 1);
        if !i mod nk = 0 then
          temp := crypt_lxor32 (subword (rotword !temp)) rcon.(!i / nk)
        else if nk > 6 && !i mod nk = 4 then
          temp := subword !temp;
        keys.(!i) <- crypt_lxor32 keys.(!i - nk) !temp;
        incr i 
      done
    with
      Invalid_argument _ as e ->
        Printf.printf "%i %i " nk (Array.length key);
        raise e*)

let shift_rows () =
  let a = get 1 0
  and b = get 1 1
  and c = get 1 2
  and d = get 1 3
  in
    put 1 0 b; put 1 1 c;
    put 1 2 d; put 1 3 a;
  let a = get 2 0
  and b = get 2 1
  and c = get 2 2
  and d = get 2 3
  in
    put 2 0 c; put 2 1 d;
    put 2 2 a; put 2 3 b;
  let a = get 3 0
  and b = get 3 1
  and c = get 3 2
  and d = get 3 3 in
    put 3 0 d; put 3 1 a;
    put 3 2 b; put 3 3 c

let inv_shift_rows () =
  let a = get 1 0
  and b = get 1 1
  and c = get 1 2
  and d = get 1 3
  in
    put 1 0 d; put 1 1 a;
    put 1 2 b; put 1 3 c;
  let a = get 2 0
  and b = get 2 1
  and c = get 2 2
  and d = get 2 3
  in
    put 2 0 c; put 2 1 d;
    put 2 2 a; put 2 3 b;
  let a = get 3 0
  and b = get 3 1
  and c = get 3 2
  and d = get 3 3
  in
    put 3 0 b; put 3 1 c;
    put 3 2 d; put 3 3 a

let mix_columns () =
  for c = 0 to 3 do
    let s'0 =
      (0x02 ** get 0 c) ++ (0x03 ** get 1 c) ++ get 2 c ++ get 3 c
    in let s'1 =
      get 0 c ++ (0x02 ** get 1 c) ++ (0x03 ** get 2 c) ++ get 3 c
    in let s'2 =
      get 0 c ++ get 1 c ++ (0x02 ** get 2 c) ++ (0x03 ** get 3 c)
    in let s'3 =
      (0x03 ** get 0 c) ++ get 1 c ++ get 2 c ++ (0x02 ** get 3 c)
    in
      put 0 c s'0;
      put 1 c s'1;
      put 2 c s'2;
      put 3 c s'3
  done

let inv_mix_columns () =
  for c = 0 to 3 do
    let s'0 =
      (0x0e ** get 0 c) ++ (0x0b ** get 1 c) ++
      (0x0d ** get 2 c) ++ (0x09 ** get 3 c)
    in let s'1 =
      (0x09 ** get 0 c) ++ (0x0e ** get 1 c) ++
      (0x0b ** get 2 c) ++ (0x0d ** get 3 c)
    in let s'2 =
      (0x0d ** get 0 c) ++ (0x09 ** get 1 c) ++
      (0x0e ** get 2 c) ++ (0x0b ** get 3 c)
    in let s'3 =
      (0x0b ** get 0 c) ++ (0x0d ** get 1 c) ++
      (0x09 ** get 2 c) ++ (0x0e ** get 3 c)
    in
      put 0 c s'0;
      put 1 c s'1;
      put 2 c s'2;
      put 3 c s'3
  done

(* Add a round key to the state. *)
let add_round_key keypos =
  let word1 = Array.unsafe_get keys keypos
  and word2 = Array.unsafe_get keys (keypos + 1)
  and word3 = Array.unsafe_get keys (keypos + 2)
  and word4 = Array.unsafe_get keys (keypos + 3) in
    let a1 = byte1_of_word word1
    and a2 = byte2_of_word word1
    and a3 = byte3_of_word word1
    and a4 = byte4_of_word word1
    and b1 = byte1_of_word word2
    and b2 = byte2_of_word word2
    and b3 = byte3_of_word word2
    and b4 = byte4_of_word word2
    and c1 = byte1_of_word word3
    and c2 = byte2_of_word word3
    and c3 = byte3_of_word word3
    and c4 = byte4_of_word word3
    and d1 = byte1_of_word word4
    and d2 = byte2_of_word word4
    and d3 = byte3_of_word word4
    and d4 = byte4_of_word word4 in
      put 0 0 (get 0 0 ++ a1); put 1 0 (get 1 0 ++ a2);
      put 2 0 (get 2 0 ++ a3); put 3 0 (get 3 0 ++ a4);
      put 0 1 (get 0 1 ++ b1); put 1 1 (get 1 1 ++ b2);
      put 2 1 (get 2 1 ++ b3); put 3 1 (get 3 1 ++ b4);
      put 0 2 (get 0 2 ++ c1); put 1 2 (get 1 2 ++ c2);
      put 2 2 (get 2 2 ++ c3); put 3 2 (get 3 2 ++ c4);
      put 0 3 (get 0 3 ++ d1); put 1 3 (get 1 3 ++ d2);
      put 2 3 (get 2 3 ++ d3); put 3 3 (get 3 3 ++ d4)

let output_from_state () =
  [| get 0 0; get 1 0; get 2 0; get 3 0;
     get 0 1; get 1 1; get 2 1; get 3 1;
     get 0 2; get 1 2; get 2 2; get 3 2;
     get 0 3; get 1 3; get 2 3; get 3 3; |]

let output_from_state_raw o p =
  bset_unsafe o p (get 0 0);
  bset_unsafe o (p + 1) (get 1 0);
  bset_unsafe o (p + 2) (get 2 0);
  bset_unsafe o (p + 3) (get 3 0);
  bset_unsafe o (p + 4) (get 0 1);
  bset_unsafe o (p + 5) (get 1 1);
  bset_unsafe o (p + 6) (get 2 1);
  bset_unsafe o (p + 7) (get 3 1);
  bset_unsafe o (p + 8) (get 0 2);
  bset_unsafe o (p + 9) (get 1 2);
  bset_unsafe o (p + 10) (get 2 2);
  bset_unsafe o (p + 11) (get 3 2);
  bset_unsafe o (p + 12) (get 0 3);
  bset_unsafe o (p + 13) (get 1 3);
  bset_unsafe o (p + 14) (get 2 3);
  bset_unsafe o (p + 15) (get 3 3)
  
let input_to_state d =
  put 0 0 d.(0); put 1 0 d.(1);
  put 2 0 d.(2); put 3 0 d.(3);
  put 0 1 d.(4); put 1 1 d.(5);
  put 2 1 d.(6); put 3 1 d.(7);
  put 0 2 d.(8); put 1 2 d.(9);
  put 2 2 d.(10); put 3 2 d.(11);
  put 0 3 d.(12); put 1 3 d.(13);
  put 2 3 d.(14); put 3 3 d.(15)

let input_to_state_raw d p =
  put 0 0 (bget_unsafe d (p + 0)); put 1 0 (bget_unsafe d (p + 1));
  put 2 0 (bget_unsafe d (p + 2)); put 3 0 (bget_unsafe d (p + 3));
  put 0 1 (bget_unsafe d (p + 4)); put 1 1 (bget_unsafe d (p + 5));
  put 2 1 (bget_unsafe d (p + 6)); put 3 1 (bget_unsafe d (p + 7));
  put 0 2 (bget_unsafe d (p + 8)); put 1 2 (bget_unsafe d (p + 9));
  put 2 2 (bget_unsafe d (p + 10)); put 3 2 (bget_unsafe d (p + 11));
  put 0 3 (bget_unsafe d (p + 12)); put 1 3 (bget_unsafe d (p + 13));
  put 2 3 (bget_unsafe d (p + 14)); put 3 3 (bget_unsafe d (p + 15))

(* Encryption cipher. Assumes key already expanded. *)
(* let cipher nr data_in =
  input_to_state data_in;
  add_round_key 0;
  for round = 1 to nr - 1 do
    sub_bytes ();
    shift_rows ();
    mix_columns ();
    add_round_key (round * 4)
  done;
  sub_bytes ();
  shift_rows ();
  add_round_key (nr * 4);
  output_from_state () *)

let cipher_raw nr data_in pos_in data_out pos_out =
  input_to_state_raw data_in pos_in;
  add_round_key 0;
  for round = 1 to nr - 1 do
    sub_bytes ();
    shift_rows ();
    mix_columns ();
    add_round_key (round * 4)
  done;
  sub_bytes ();
  shift_rows ();
  add_round_key (nr * 4);
  output_from_state_raw data_out pos_out

(* Decryption cipher. Assumes key already expanded. *)
let inv_cipher_raw nr data_in pos_in data_out pos_out =
  input_to_state_raw data_in pos_in;
  add_round_key (nr * 4);
  for round = (nr - 1) downto 1 do
    inv_shift_rows ();
    inv_sub_bytes ();
    add_round_key (round * 4);
    inv_mix_columns ();
  done;
  inv_shift_rows ();
  inv_sub_bytes ();
  add_round_key 0;
  output_from_state_raw data_out pos_out

let _ = Random.self_init ()

(* Pad the input data (RFC2898, PKCS #5), then encrypt using a 16 byte AES
cipher in cipher block chaining mode, with a random initialisation vector, which
is stored as the first 16 bytes of the result. *)
let ran255 () =
  Random.int 255

let mkiv () =
  let r = ran255 in
    [| r (); r (); r (); r ();
       r (); r (); r (); r ();
       r (); r (); r (); r ();
       r (); r (); r (); r () |]

(* Debug function to print a block as characters. *)
let print_block arr =
  Array.iter (fun i -> Printf.printf "%c" (char_of_int i)) arr;
  flprint "\n\n"

(* Build blocks for encryption, including padding. *)
let get_blocks data =
  let l = bytes_size data in
    let fullblocks =
      if l < 16 then [] else
        let blocks = ref [] in
          for x = 0 to l / 16 - 1 do
            blocks =|
              let a = Array.make 16 0 in
                for y = 0 to 15 do
                  Array.unsafe_set a y (bget_unsafe data (x * 16 + y))
                done;
                a (*Array.init 16 (fun y -> bget data (x * 16 + y))*)
          done;
          rev !blocks
    in let lastblock =
      let getlast n =
        if n = 0 then [] else
          let bytes = ref [] in
            for x = 0 to n - 1 do
              bytes =| bget data (l - 1 - x)
            done;
            !bytes
      in let pad n =
        many n n
      in
        let overflow = l mod 16 in
          Array.of_list (getlast overflow @ pad (16 - overflow))
    in
      fullblocks @ [lastblock]

(* Flatten a list of blocks into a bytes *)
let bytes_of_blocks blocks =
  let len = 16 * length blocks in
    let s = mkbytes len
    in let p = ref 0 in
      iter
        (fun a ->
          Array.iter (fun v -> bset s !p v; incr p) a)
        blocks;
      s

(* These two functions strip the padding from a stream once it's been decoded.*)
let get_padding s =
  let l = bytes_size s in
    assert (l >= 16);
    let potential = bget s (l - 1) in
      if potential > 0x10 || potential < 0x01 then None else
        let rec elts_equal p f t =
          if f = t then p = bget s t else
            p = bget s f && elts_equal p (f + 1) t
        in
          if elts_equal potential (l - potential) (l - 1)
            then Some potential
            else None

let cutshort s =
  if bytes_size s = 0 then mkbytes 0 else
    if bytes_size s <= 16 then s else
      match get_padding s with
      | None -> s
      | Some padding ->
          let s' = mkbytes (bytes_size s - padding) in
            for x = 0 to bytes_size s' - 1 do
              bset_unsafe s' x (bget_unsafe s x)
            done;
            s'

(* Decrypt data *)
let print_txt d p =
  for x = p to p + 15 do Printf.printf "%02x" (bget d x) done; flprint "\n"

let aes_decrypt_data ?(remove_padding = true) nk key data =
  key_expansion nk key;
  let len = bytes_size data in
    if len <= 16 then mkbytes 0 else
      let output = mkbytes (len - 16)
      and prev_ciphertext = mkbytes 16 in
        for x = 0 to 15 do bset_unsafe prev_ciphertext x (bget_unsafe data x) done;
        let pos = ref 16 in
          while !pos < len do
            inv_cipher_raw (nk + 6) data !pos output (!pos - 16);
            for x = 0 to 15 do
              bset_unsafe output (x + !pos - 16) (bget_unsafe prev_ciphertext x lxor bget_unsafe output (x + !pos - 16));
              bset_unsafe prev_ciphertext x (bget_unsafe data (x + !pos))
            done;
            pos += 16
          done;
          if remove_padding then cutshort output else output

(* With ECB instead. Data on input must be a multiple of 16. *)
let aes_decrypt_data_ecb ?(remove_padding = true) nk key data =
  key_expansion nk key;
  let size = bytes_size data in
    if size = 0 then mkbytes 0 else
      let output = mkbytes size
      and pos = ref 0 in
        while !pos < size do
          inv_cipher_raw (nk + 6) data !pos output !pos;
          pos += 16;
        done;
        (if remove_padding then cutshort else ident) output

(* Encrypt data *)
let aes_encrypt_data ?(firstblock = mkiv ()) nk key data =
  let key = key_expansion nk key in
  let outblocks = ref [] in
    let prev_ciphertext = ref firstblock in
      iter
        (fun block ->
          let ciphertext =
            let src = string_of_int_array ((array_map2 (lxor)) block !prev_ciphertext)
            and dst = String.make 16 ' ' in
            (*cipher (nk + 6) ((array_map2 (lxor)) block !prev_ciphertext);*)
            aes_encrypt key src 0 dst 0;
            (int_array_of_string dst)
          in
            prev_ciphertext := ciphertext;
            outblocks =| ciphertext)
        (get_blocks data);
        bytes_of_blocks (firstblock::rev !outblocks)

(* With ECB instead. Input length is multiple of 16. *)
let aes_encrypt_data_ecb nk key data =
  key_expansion nk key;
  let size = bytes_size data in
    if size = 0 then mkbytes 0 else
      let output = mkbytes size
      and pos = ref 0 in
        while !pos < size do
          cipher_raw (nk + 6) data !pos output !pos;
          pos += 16
        done;
        output

(* SHA-256. Message length must be a multiple of 8 bits long. *)
let k =
 [| 0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l; 0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
    0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l; 0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
    0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl; 0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
    0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l; 0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
    0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l; 0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
    0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l; 0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
    0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l; 0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
    0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l; 0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l |]

let ch x y z = lxor32 (land32 x y) (land32 (lnot32 x) z)

let maj x y z = lxor32 (land32 x y) (lxor32 (land32 x z) (land32 y z))

let sum0 x =
  lxor32
    (lor32 (lsr32 x 2) (lsl32 x (32 - 2)))
    (lxor32 (lor32 (lsr32 x 13) (lsl32 x (32 - 13)))
            (lor32 (lsr32 x 22) (lsl32 x (32 - 22))))

let sum1 x =
  lxor32
    (lor32 (lsr32 x 6) (lsl32 x (32 - 6)))
    (lxor32 (lor32 (lsr32 x 11) (lsl32 x (32 - 11)))
            (lor32 (lsr32 x 25) (lsl32 x (32 - 25))))

let lsig0 x =
  lxor32
    (lor32 (lsr32 x 7) (lsl32 x (32 - 7)))
    (lxor32 (lor32 (lsr32 x 18) (lsl32 x (32 - 18)))
            (lor32 (lsr32 x 3) (lsr32 x 3)))

let lsig1 x =
  lxor32
    (lor32 (lsr32 x 17) (lsl32 x (32 - 17)))
    (lxor32 (lor32 (lsr32 x 19) (lsl32 x (32 - 19)))
            (lor32 (lsr32 x 10) (lsr32 x 10)))

let sha_init =
  [| 0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al; 0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l |]

let warr = Array.make 64 0l

let harr = Array.make 8 0l

let a = ref 0l
let b = ref 0l
let c = ref 0l
let d = ref 0l
let e = ref 0l
let f = ref 0l
let g = ref 0l
let h = ref 0l
let t1 = ref 0l
let t2 = ref 0l

(* 1. Preprocess message. Takes an input and returns a list of 512 bit (16 32-bit bytes long) arrays. *)
let preprocess i =
  (* a. list of bytes from i *)
  let rec getbytes sofar =
    match i.Pdfio.input_byte () with
    | x when x = Pdfio.no_more -> (rev sofar)
    | n -> getbytes (n::sofar)
  in
    let mkword a b c d =
      lor32
        (lor32 (lsl32 (i32ofi a) 24) (lsl32 (i32ofi b) 16))
        (lor32 (lsl32 (i32ofi c) 8) (i32ofi d))
    in
      (* Group into groups of 64 or less. Each group will be a 512-bit block. *)
      let lastgroups bytes totallength =
        let length_in_bits = i64mul (i64ofi totallength) 8L in
          let pokelength arr =
            arr.(14) <- i32ofi64 (lsr64 (land64 length_in_bits 0xFFFFFFFF00000000L) 32);
            arr.(15) <- i32ofi64 (land64 length_in_bits 0x00000000FFFFFFFFL)
          in
            if length bytes <= 55 then
              (* We have space for the whole thing in one block: 55 bytes of data, plus the '1', plus 8 bytes of length *)
              let arr = Array.make 16 0l
              and bytes' = bytes @ [128]
              and pos = ref 0 in
                let rec mkwords = function
                  | a::b::c::d::r -> arr.(!pos) <- mkword a b c d; incr pos; mkwords r
                  | [a; b; c] -> arr.(!pos) <- mkword a b c 0
                  | [a; b] -> arr.(!pos) <- mkword a b 0 0
                  | [a] -> arr.(!pos) <- mkword a 0 0 0
                  | [] -> ()
                in
                  mkwords bytes';
                  pokelength arr;
                  [arr]
            else
              (* We don't have space. Put the data and the '1' into the first block, and the
              length into the second block *)
              let arr = Array.make 16 0l
              and arr2 = Array.make 16 0l
              and bytes' = if length bytes = 64 then bytes else bytes @ [128]
              and pos = ref 0 in
                let rec mkwords = function
                  | a::b::c::d::r -> arr.(!pos) <- mkword a b c d; incr pos; mkwords r
                  | [a; b; c] -> arr.(!pos) <- mkword a b c 0
                  | [a; b] -> arr.(!pos) <- mkword a b 0 0
                  | [a] -> arr.(!pos) <- mkword a 0 0 0
                  | [] -> ()
                in
                  mkwords bytes';
                  if length bytes = 64 then arr2.(0) <- mkword 128 0 0 0;
                  pokelength arr2;
                  [arr; arr2]
      in let normalgroup bytes =
        (* in: 64 bytes: output: 16-length array of int32s *)
        let arr = Array.make 16 0l
        and pos = ref 0 in
          let rec mkwords = function
            | a::b::c::d::r ->
                arr.(!pos) <- mkword a b c d; incr pos; mkwords r
            | [] -> ()
            | _ -> assert false
          in
            mkwords bytes; arr
      in
        let bytes = getbytes [] in
          match rev (splitinto_small 64 bytes) with
          | [] -> lastgroups [] (length bytes)
          | h::t -> rev (rev (lastgroups h (length bytes)) @ map normalgroup t)

(* Main SHA256 process, given some 512bit blocks *)
let sha_process mblocks =
  Array.blit sha_init 0 harr 0 8;
  iter
    (function m ->
       for t = 0 to 15 do warr.(t) <- m.(t) done;
       for t = 16 to 63 do
         warr.(t) <-
           i32add
             (i32add (lsig1 warr.(t - 2)) warr.(t - 7))
             (i32add (lsig0 warr.(t - 15)) warr.(t - 16))
       done;
       a := harr.(0); b := harr.(1); c := harr.(2); d := harr.(3);
       e := harr.(4); f := harr.(5); g := harr.(6); h := harr.(7);
       for t = 0 to 63 do
         t1 := i32add (i32add !h (sum1 !e)) (i32add (i32add (ch !e !f !g) k.(t)) warr.(t));
         t2 := i32add (sum0 !a) (maj !a !b !c);
         h := !g; g := !f; f := !e; e := i32add !d !t1;
         d := !c; c := !b; b := !a; a := i32add !t1 !t2;
       done;
       harr.(0) <- i32add !a harr.(0); harr.(1) <- i32add !b harr.(1);
       harr.(2) <- i32add !c harr.(2); harr.(3) <- i32add !d harr.(3);
       harr.(4) <- i32add !e harr.(4); harr.(5) <- i32add !f harr.(5);
       harr.(6) <- i32add !g harr.(6); harr.(7) <- i32add !h harr.(7))
    mblocks;
  [|harr.(0); harr.(1); harr.(2); harr.(3); harr.(4); harr.(5); harr.(6); harr.(7)|]

let sha256 (i : Pdfio.input) =
  let words = sha_process (preprocess i) in
    let s = Array.make 32 0 in
      for x = 0 to 7 do
        s.(x * 4) <- i32toi (lsr32 (land32 words.(x) 0xFF000000l) 24);
        s.(x * 4 + 1) <- i32toi (lsr32 (land32 words.(x) 0x00FF0000l) 16);
        s.(x * 4 + 2) <- i32toi (lsr32 (land32 words.(x) 0x0000FF00l) 8);
        s.(x * 4 + 3) <- i32toi (land32 words.(x) 0x000000FFl)
      done;
      string_of_int_array s

(* SHA-384 / SHA-512 *)
let k =
  [| 0x428a2f98d728ae22L; 0x7137449123ef65cdL; 0xb5c0fbcfec4d3b2fL; 0xe9b5dba58189dbbcL;
     0x3956c25bf348b538L; 0x59f111f1b605d019L; 0x923f82a4af194f9bL; 0xab1c5ed5da6d8118L;
     0xd807aa98a3030242L; 0x12835b0145706fbeL; 0x243185be4ee4b28cL; 0x550c7dc3d5ffb4e2L;
     0x72be5d74f27b896fL; 0x80deb1fe3b1696b1L; 0x9bdc06a725c71235L; 0xc19bf174cf692694L;
     0xe49b69c19ef14ad2L; 0xefbe4786384f25e3L; 0x0fc19dc68b8cd5b5L; 0x240ca1cc77ac9c65L;
     0x2de92c6f592b0275L; 0x4a7484aa6ea6e483L; 0x5cb0a9dcbd41fbd4L; 0x76f988da831153b5L;
     0x983e5152ee66dfabL; 0xa831c66d2db43210L; 0xb00327c898fb213fL; 0xbf597fc7beef0ee4L;
     0xc6e00bf33da88fc2L; 0xd5a79147930aa725L; 0x06ca6351e003826fL; 0x142929670a0e6e70L;
     0x27b70a8546d22ffcL; 0x2e1b21385c26c926L; 0x4d2c6dfc5ac42aedL; 0x53380d139d95b3dfL;
     0x650a73548baf63deL; 0x766a0abb3c77b2a8L; 0x81c2c92e47edaee6L; 0x92722c851482353bL;
     0xa2bfe8a14cf10364L; 0xa81a664bbc423001L; 0xc24b8b70d0f89791L; 0xc76c51a30654be30L;
     0xd192e819d6ef5218L; 0xd69906245565a910L; 0xf40e35855771202aL; 0x106aa07032bbd1b8L;
     0x19a4c116b8d2d0c8L; 0x1e376c085141ab53L; 0x2748774cdf8eeb99L; 0x34b0bcb5e19b48a8L;
     0x391c0cb3c5c95a63L; 0x4ed8aa4ae3418acbL; 0x5b9cca4f7763e373L; 0x682e6ff3d6b2b8a3L;
     0x748f82ee5defb2fcL; 0x78a5636f43172f60L; 0x84c87814a1f0ab72L; 0x8cc702081a6439ecL;
     0x90befffa23631e28L; 0xa4506cebde82bde9L; 0xbef9a3f7b2c67915L; 0xc67178f2e372532bL;
     0xca273eceea26619cL; 0xd186b8c721c0c207L; 0xeada7dd6cde0eb1eL; 0xf57d4f7fee6ed178L;
     0x06f067aa72176fbaL; 0x0a637dc5a2c898a6L; 0x113f9804bef90daeL; 0x1b710b35131c471bL;
     0x28db77f523047d84L; 0x32caab7b40c72493L; 0x3c9ebe0a15c9bebcL; 0x431d67c49c100d4cL;
     0x4cc5d4becb3e42b6L; 0x597f299cfc657e2aL; 0x5fcb6fab3ad6faecL; 0x6c44198c4a475817L |]

let rotate x n = lor64 (lsr64 x n) (lsl64 x (64 - n))

let shift = lsr64

let ch x y z = lxor64 (land64 x y) (land64 (lnot64 x) z)

let maj x y z = lxor64 (land64 x y) (lxor64 (land64 x z) (land64 y z))

let sum0 x = lxor64 (rotate x 28) (lxor64 (rotate x 34) (rotate x 39))

let sum1 x = lxor64 (rotate x 14) (lxor64 (rotate x 18) (rotate x 41))

let lsig0 x = lxor64 (rotate x 1) (lxor64 (rotate x 8) (shift x 7))

let lsig1 x = lxor64 (rotate x 19) (lxor64 (rotate x 61) (shift x 6))

let warr = Array.make 80 0L

let harr = Array.make 8 0L

let a = ref 0L
let b = ref 0L
let c = ref 0L
let d = ref 0L
let e = ref 0L
let f = ref 0L
let g = ref 0L
let h = ref 0L
let t1 = ref 0L
let t2 = ref 0L

let sha_init_384 =
  [| 0xcbbb9d5dc1059ed8L; 0x629a292a367cd507L; 0x9159015a3070dd17L; 0x152fecd8f70e5939L;
     0x67332667ffc00b31L; 0x8eb44a8768581511L; 0xdb0c2e0d64f98fa7L; 0x47b5481dbefa4fa4L |]

let sha_init_512 =
  [| 0x6a09e667f3bcc908L; 0xbb67ae8584caa73bL; 0x3c6ef372fe94f82bL; 0xa54ff53a5f1d36f1L;
     0x510e527fade682d1L; 0x9b05688c2b3e6c1fL; 0x1f83d9abfb41bd6bL; 0x5be0cd19137e2179L |]

(* 1. Preprocess message. Takes an input and returns a list of 1024 bit (16 64-bit bytes long) arrays. *)
let preprocess_512 i =
  (* a. list of bytes from i *)
  let rec getbytes sofar =
    match i.Pdfio.input_byte () with
    | x when x = Pdfio.no_more -> (rev sofar)
    | n -> getbytes (n::sofar)
  in
    let mkword a b c d e f g h =
      lor64
        (lor64
          (lor64 (lsl64 (i64ofi a) 56) (lsl64 (i64ofi b) 48))
          (lor64 (lsl64 (i64ofi c) 40) (lsl64 (i64ofi d) 32)))
        (lor64
          (lor64 (lsl64 (i64ofi e) 24) (lsl64 (i64ofi f) 16))
          (lor64 (lsl64 (i64ofi g) 8) (i64ofi h)))
    in
      (* Group into groups of 64 or less. Each group will be a 1024-bit block. *)
      let lastgroups bytes totallength =
        let length_in_bits = i64mul (i64ofi totallength) 8L in
          let pokelength arr =
            arr.(14) <- lsr64 (land64 length_in_bits 0xFFFFFFFF00000000L) 32;
            arr.(15) <- land64 length_in_bits 0x00000000FFFFFFFFL
          in
            if length bytes <= 111 then
              (* We have space for the whole thing in one block: 119 bytes of data, plus the '1', plus 8 bytes of length *)
              let arr = Array.make 16 0L
              and bytes' = bytes @ [128]
              and pos = ref 0 in
                let rec mkwords = function
                  | a::b::c::d::e::f::g::h::r -> arr.(!pos) <- mkword a b c d e f g h; incr pos; mkwords r
                  | [a; b; c; d; e; f; g] -> arr.(!pos) <- mkword a b c d e f g 0
                  | [a; b; c; d; e; f] -> arr.(!pos) <- mkword a b c d e f 0 0
                  | [a; b; c; d; e] -> arr.(!pos) <- mkword a b c d e 0 0 0
                  | [a; b; c; d] -> arr.(!pos) <- mkword a b c d 0 0 0 0
                  | [a; b; c] -> arr.(!pos) <- mkword a b c 0 0 0 0 0
                  | [a; b] -> arr.(!pos) <- mkword a b 0 0 0 0 0 0 
                  | [a] -> arr.(!pos) <- mkword a 0 0 0 0 0 0 0
                  | [] -> ()
                in
                  mkwords bytes';
                  pokelength arr;
                  [arr]
            else
              (* We don't have space. Put the data and the '1' into the first block, and the
              length into the second block *)
              let arr = Array.make 16 0L
              and arr2 = Array.make 16 0L
              and bytes' = if length bytes = 128 then bytes else bytes @ [128]
              and pos = ref 0 in
                let rec mkwords = function
                  | a::b::c::d::e::f::g::h::r -> arr.(!pos) <- mkword a b c d e f g h; incr pos; mkwords r
                  | [a; b; c; d; e; f; g] -> arr.(!pos) <- mkword a b c d e f g 0
                  | [a; b; c; d; e; f] -> arr.(!pos) <- mkword a b c d e f 0 0
                  | [a; b; c; d; e] -> arr.(!pos) <- mkword a b c d e 0 0 0
                  | [a; b; c; d] -> arr.(!pos) <- mkword a b c d 0 0 0 0
                  | [a; b; c] -> arr.(!pos) <- mkword a b c 0 0 0 0 0
                  | [a; b] -> arr.(!pos) <- mkword a b 0 0 0 0 0 0 
                  | [a] -> arr.(!pos) <- mkword a 0 0 0 0 0 0 0
                  | [] -> ()
                in
                  mkwords bytes';
                (*iter
                  (function l -> arr.(!pos) <- mkword l; incr pos)
                  (splitinto_small 8 bytes');*)
                if length bytes = 128 then arr2.(0) <- mkword 128 0 0 0 0 0 0 0;
                pokelength arr2;
                [arr; arr2]
      in let normalgroup bytes =
        (* in: 128 bytes: output: 16-length array of int64s *)
        let arr = Array.make 16 0L
        and pos = ref 0 in
          let rec mkwords = function
            | a::b::c::d::e::f::g::h::r ->
                arr.(!pos) <- mkword a b c d e f g h; incr pos; mkwords r
            | [] -> ()
            | _ -> assert false
          in
            mkwords bytes; arr
      in
        let bytes = getbytes [] in
          match rev (splitinto_small 128 bytes) with
          | [] -> lastgroups [] (length bytes)
          | h::t -> rev (rev (lastgroups h (length bytes)) @ map normalgroup t)

(* Main SHA512 process, given some 512bit blocks *)
let sha_process_512 is384 mblocks =
  (*flprint "sha_process_512";*)
  Array.blit (if is384 then sha_init_384 else sha_init_512) 0 harr 0 8;
  iter
    (function m ->
       for t = 0 to 15 do warr.(t) <- m.(t) done;
       for t = 16 to 79 do
         warr.(t) <-
           i64add
             (i64add (lsig1 warr.(t - 2)) warr.(t - 7))
             (i64add (lsig0 warr.(t - 15)) warr.(t - 16))
       done;
       a := harr.(0); b := harr.(1); c := harr.(2); d := harr.(3);
       e := harr.(4); f := harr.(5); g := harr.(6); h := harr.(7);
       for t = 0 to 79 do
         t1 := i64add (i64add !h (sum1 !e)) (i64add (i64add (ch !e !f !g) k.(t)) warr.(t));
         t2 := i64add (sum0 !a) (maj !a !b !c);
         h := !g; g := !f; f := !e; e := i64add !d !t1;
         d := !c; c := !b; b := !a; a := i64add !t1 !t2;
       done;
       harr.(0) <- i64add !a harr.(0); harr.(1) <- i64add !b harr.(1);
       harr.(2) <- i64add !c harr.(2); harr.(3) <- i64add !d harr.(3);
       harr.(4) <- i64add !e harr.(4); harr.(5) <- i64add !f harr.(5);
       harr.(6) <- i64add !g harr.(6); harr.(7) <- i64add !h harr.(7))
    mblocks;
  [|harr.(0); harr.(1); harr.(2); harr.(3); harr.(4); harr.(5); harr.(6); harr.(7) |]
  
let sha512_inner ?(is384 = false) (i : Pdfio.input) =
  let words = sha_process_512 is384 (preprocess_512 i) in
    (*Printf.printf "sha512 post process: %i words\n" (Array.length words);*)
    (* Select byte n from x. n = 0 to 7 (0 highest) *)
    let select_byte n x =
      let p = (7 - n) * 8 in
        i64toi (lsr64 (land64 x (lsl64 0xffL p)) p);
    in
      let s = Array.make 64 0 in
        for x = 0 to 7 do
          (* From each 64 bit word, make 8 bytes *)
          for n = 0 to 7 do
            s.(x * 8 + n) <- select_byte n words.(x)
          done
        done;
        if is384
         then String.sub (string_of_int_array s) 0 (6 * 8) 
         else string_of_int_array s

let sha512 = sha512_inner ~is384:false

let sha384 = sha512_inner ~is384:true

