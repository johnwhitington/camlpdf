(* Encryption and Decryption *)
open Pdfutil
open Pdfio

let crypt_debug = ref false

(* ARC4 = old style or crypt filter with V2. AESV2 = Crypt filter with AESV2.
AESV3 is 256 bit AES encryption from the PDF 1.7 Extensions, or the new ISO one. *)
type encryption =
  | ARC4 of int * int (* keylength, r (= 2 or 3 or 4) *)
  | AESV2 (* v = 4, r = 4 *)
  | AESV3 of bool (* v = 5, r = 5 or v = 5, r = 6 if true *)

(* Given an object number, generation number, input key and key length in bits,
apply Algorithm 3.1 from the PDF Reference manual to obtain the hash to be used
by the encryption function. *)
let find_hash crypt_type obj gen key keylength =
  let from_obj =
    [| i32toi (land32 obj 0x000000ffl);
       i32toi (lsr32 (land32 obj 0x0000ff00l) 8);
       i32toi (lsr32 (land32 obj 0x00ff0000l) 16) |]
  in let from_gen =
    [| i32toi (land32 gen 0x000000ffl);
       i32toi (lsr32 (land32 gen 0x0000ff00l) 8) |]
  in let extra =
    if crypt_type = AESV2 then [| 0x73; 0x41; 0x6C; 0x54 |] else [| |]
  in
    let digest_input = string_of_int_arrays [key; from_obj; from_gen; extra] in
      int_array_of_string
        (String.sub (Digest.string digest_input) 0 (min 16 (keylength / 8 + 5)))

(* Find a key, given a password, O entry, P entry, id entry, and key length in
bits. *)
let paddings =
  [| 0x28; 0xbf; 0x4e; 0x5e; 0x4e; 0x75; 0x8a; 0x41;
     0x64; 0x00; 0x4e; 0x56; 0xff; 0xfa; 0x01; 0x08;
     0x2e; 0x2e; 0x00; 0xb6; 0xd0; 0x68; 0x3e; 0x80;
     0x2f; 0x0c; 0xa9; 0xfe; 0x64; 0x53; 0x69; 0x7a |]

let pad_password password =
  let pw = Array.make 32 0 in
    Array.iteri (fun i v -> if i < 32 then pw.(i) <- v) password;
    let n = Array.length password in
      if n < 32 then
        for x = n to 31 do
          pw.(x) <- paddings.(x - n)
        done;
  pw

let find_key no_encrypt_metadata password r o p id keylength =
  let password = int_array_of_string password
  in let o = int_array_of_string o
  in let id = int_array_of_string id in
    let pw = pad_password password in
      let from_p =
        [| i32toi (land32 p 0x000000ffl);
           i32toi (lsr32 (land32 p 0x0000ff00l) 8);
           i32toi (lsr32 (land32 p 0x00ff0000l) 16);
           i32toi (lsr32 (land32 p 0xff000000l) 24) |]
      in let rev4_no_metadata =
        if r >= 4 && no_encrypt_metadata then [|255; 255; 255; 255|] else [||]
      in
        let todigest = [pw; o; from_p; id; rev4_no_metadata] in
          let hash_input = string_of_int_arrays todigest in
            let hashed = Digest.string hash_input in
              let hashed' =
                if r >= 3 then
                  let h = ref hashed in
                    for x = 1 to 50 do
                      let hashed = Digest.string !h in
                        h :=
                          string_of_int_array
                            (Array.sub (int_array_of_string hashed) 0 (keylength / 8))
                    done;
                    !h
                else
                  hashed
              in
                Array.sub (int_array_of_string hashed') 0 (keylength / 8)

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
let key_expansion nk key =
  try
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
        raise e

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
let cipher nr data_in =
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
  output_from_state ()

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
  key_expansion nk key;
  let outblocks = ref [] in
    let prev_ciphertext = ref firstblock in
      iter
        (fun block ->
          let ciphertext =
            cipher (nk + 6) ((array_map2 (lxor)) block !prev_ciphertext)
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

(* Authenticate the user password, given the password string and U, O, P, id
and key length entry. *)
let authenticate_user no_encrypt_metadata password r u o p id keylength =
(*flprint "AUTHENTICATE_USER\n";
  Printf.printf "no_encrypt_metadata: %b\n" no_encrypt_metadata;
  Printf.printf "user_pw = |%s|\n" password;
  Printf.printf "r = %i\n" r;
  Printf.printf "u = %s\n" u;
  Printf.printf "o = %s\n" o;
  Printf.printf "p = %li\n" p;
  Printf.printf "id = %s\n" id;
  Printf.printf "keylength = %i\n" keylength;
flprint "END_AUTHENTICATE_USER\n";*)
  let u = int_array_of_string u in
    let key = find_key no_encrypt_metadata password r o p id keylength in
      if r >= 3 then
        let id = int_array_of_string id in
          let todigest = [paddings; id] in
            let hash_input = string_of_int_arrays todigest in
              let hashed = Digest.string hash_input in
                let encrypted_hashed =
                  int_array_of_bytes (crypt key (bytes_of_string hashed))
                in
                  let u' = ref [||] in
                    u' := encrypted_hashed;
                    for x = 1 to 19 do
                      let key' = Array.make (keylength / 8) 0 in
                        for k = 0 to (keylength / 8) - 1 do
                          key'.(k) <- key.(k) lxor x 
                        done;
                        u' :=
                          int_array_of_bytes
                            (crypt key' (bytes_of_int_array !u'))
                    done;
                    Array.sub u 0 16 = !u'
      else
        u = int_array_of_bytes (crypt key (bytes_of_int_array paddings))

(* Decrypt a PDF file, given the user password. *)
let string_of_pdf : (Pdf.pdfobject -> string) ref = ref (function _ -> "")

let rec decrypt crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r file_encryption_key l =
  (*Printf.printf "decrypt %s" (!string_of_pdf l); flprint "\n";*)
  match l with
  | Pdf.String s ->
      (* Avoid decrypting an object which came from an object stream, since the
      object stream has been decrypted en-masse already. *)
      begin match fst (Pdf.pdfobjmap_find obj pdf.Pdf.objects.Pdf.pdfobjects) with
      | {contents = Pdf.ParsedAlreadyDecrypted _} -> Pdf.String s
      | _ (* Will always be Parsed for now...*) ->
        let f =
          (if crypt_type = AESV2 then
            (if encrypt then aes_encrypt_data 4 else aes_decrypt_data 4)
           else if (match crypt_type with AESV3 _ -> true | _ -> false) then
            (if encrypt then aes_encrypt_data 8 else aes_decrypt_data 8)
           else
            crypt)
        in
          let s_ints = bytes_of_string s in
            if r = 5 || r = 6 then
              let key = match file_encryption_key with Some k -> k | None -> raise (Pdf.PDFError "decrypt: no key B") in
                Pdf.String (string_of_bytes (f (int_array_of_string key) s_ints))
            else
              let hash = find_hash crypt_type (i32ofi obj) (i32ofi gen) key keylength in
                Pdf.String (string_of_bytes (f hash s_ints))
      end
  | (Pdf.Stream _) as stream ->
      decrypt_stream crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r file_encryption_key stream
  | Pdf.Array a ->
      begin match fst (Pdf.pdfobjmap_find obj pdf.Pdf.objects.Pdf.pdfobjects) with
      | {contents = Pdf.ParsedAlreadyDecrypted _} -> Pdf.Array a
      | _ -> Pdf.recurse_array (decrypt crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r file_encryption_key) a
      end
  | Pdf.Dictionary d ->
      begin match fst (Pdf.pdfobjmap_find obj pdf.Pdf.objects.Pdf.pdfobjects) with
      | {contents = Pdf.ParsedAlreadyDecrypted _} -> Pdf.Dictionary d
      | _ -> Pdf.recurse_dict (decrypt crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r file_encryption_key) d
      end
  | x -> x

and decrypt_stream crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r file_encryption_key stream =
  Pdf.getstream stream;
  begin match stream with
  | (Pdf.Stream {contents = (Pdf.Dictionary dict as d, Pdf.Got data)}) as stream ->
      if
        begin let identity_crypt_filter_present =
          match Pdf.lookup_direct pdf "/Filter" d with
          | Some (Pdf.Name "/Crypt")
          | Some (Pdf.Array (Pdf.Name "/Crypt"::_)) ->
              begin match Pdf.lookup_direct pdf "/DecodeParms" d with
              | Some (Pdf.Dictionary decodeparmsdict)
              | Some (Pdf.Array (Pdf.Dictionary decodeparmsdict::_)) ->
                  begin match
                    Pdf.lookup_direct pdf "/Name" (Pdf.Dictionary decodeparmsdict)
                  with
                  | Some (Pdf.Name "/Identity") | None -> true
                  | _ -> false
                  end
              | _ -> true
              end
          | _ -> false
        in
          (no_encrypt_metadata &&
             (match Pdf.lookup_direct pdf "/Type" d with Some (Pdf.Name "/Metadata") -> true | _ -> false))
          || identity_crypt_filter_present
        end
      then
        stream
      else
        let data' =
          let f =
            (if crypt_type = AESV2 then
               (if encrypt then aes_encrypt_data 4 else aes_decrypt_data 4)
             else if (match crypt_type with AESV3 _ -> true | _ -> false) then
               (if encrypt then aes_encrypt_data 8 else aes_decrypt_data 8)
             else
               crypt)
          in
            if r = 5 || r = 6 then
              let key = match file_encryption_key with Some k -> k | None -> raise (Pdf.PDFError "decrypt: no key C") in
                f (int_array_of_string key) data
            else
              let hash = find_hash crypt_type (i32ofi obj) (i32ofi gen) key keylength in
                f hash data
        in let dict' =
          Pdf.recurse_dict
            (decrypt crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r file_encryption_key) dict
        in
          let dict'' =
            if bytes_size data <> bytes_size data' then
              Pdf.replace_dict_entry
                dict' "/Length" (Pdf.Integer (bytes_size data'))
            else
              dict'
          in
            Pdf.Stream {contents = (dict'', Pdf.Got data')}
  | _ -> assert false
  end

let process_cryption no_encrypt_metadata encrypt pdf crypt_type user_pw r u o p id keylength file_encryption_key =
  (*Printf.printf "no_encrypt_metadata: %b\n" no_encrypt_metadata;
  Printf.printf "encrypt = %b\n" encrypt;
  Printf.printf "crypt_type = %s\n" ((function ARC4 (a, b) -> Printf.sprintf "ARC4 %i, %i" a b | AESV2 -> "AESV2" | AESV3 -> "AESV3") crypt_type);
  Printf.printf "user_pw = |%s|\n" user_pw;
  Printf.printf "r = %i\n" r;
  Printf.printf "u = %s\n" u;
  Printf.printf "o = %s\n" o;
  Printf.printf "p = %li\n" p;
  Printf.printf "id = %s\n" id;
  Printf.printf "keylength = %i\n" keylength;*)
  let encryption_object_number =
    match pdf.Pdf.trailerdict with
    | Pdf.Dictionary d ->
        begin match lookup "/Encrypt" d with
        | Some (Pdf.Indirect i) -> i
        | _ -> -1
        end
    | _ -> -1
  in
  let do_encryption key =
    Pdf.objiter_gen
      (fun objnum gennum obj ->
         if objnum <> encryption_object_number then
           begin
             ignore
              (Pdf.addobj_given_num
                 pdf
                 (objnum, decrypt crypt_type pdf no_encrypt_metadata encrypt objnum gennum key keylength r file_encryption_key obj))
           end)
      pdf;
    let trailerdict' = Pdf.remove_dict_entry pdf.Pdf.trailerdict "/Encrypt" in
      pdf.Pdf.trailerdict <- trailerdict';
      Some pdf
  in
    if r = 5 || r = 6 then (* AESV3 *)
      begin match file_encryption_key with
      | Some k -> do_encryption (int_array_of_string k)
      | None -> None
      end
    else if authenticate_user no_encrypt_metadata user_pw r u o p id keylength then
      do_encryption (find_key no_encrypt_metadata user_pw r o p id keylength)
    else None

let printable_of_string s =
  String.concat "" (map (fun c -> Printf.sprintf "%02x" (int_of_char c)) (explode s))

let get_encryption_values pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> raise (Pdf.PDFError "get_encryption_values called on unencrypted pdf")
  | Some encryptdict ->
      let crypt_type =
        match
          Pdf.lookup_direct pdf "/Filter" encryptdict,
          Pdf.lookup_direct pdf "/V" encryptdict,
          Pdf.lookup_direct pdf "/Length" encryptdict,
          Pdf.lookup_direct pdf "/R" encryptdict
        with
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 1), _, Some (Pdf.Integer r)
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 2), None, Some (Pdf.Integer r) ->
            Some (ARC4 (40, r))
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 2), Some (Pdf.Integer n), _
            when n mod 8 = 0 && n >= 40 && n <= 128 ->
              Some (ARC4 (n, 3))
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer (4 | 5)), length, Some (Pdf.Integer r) ->
            begin match Pdf.lookup_direct pdf "/CF" encryptdict with
            | Some cfdict ->
                begin match Pdf.lookup_direct pdf "/StdCF" cfdict with
                | Some stdcfdict ->
                    begin match Pdf.lookup_direct pdf "/CFM" stdcfdict with
                    | Some (Pdf.Name "/V2") ->
                        begin match length with
                        | Some (Pdf.Integer i) -> Some (ARC4 (i, 4))
                        | _ ->
                            begin match Pdf.lookup_direct pdf "/Length" cfdict with
                            | Some (Pdf.Integer i) -> Some (ARC4 (i, 4))
                            | _ -> None
                            end
                        end
                    | Some (Pdf.Name "/AESV2") -> Some AESV2
                    | Some (Pdf.Name "/AESV3") ->
                        Some (AESV3 (r = 6))
                    | _ -> None
                    end
                | _ -> None
                end
            | _ -> None
            end
        | _ -> None
      in
        match crypt_type with
        | None -> raise (Pdf.PDFError "No encryption method")
        | Some crypt_type ->
            let o =
              match Pdf.lookup_direct pdf "/O" encryptdict with
              | Some (Pdf.String o) -> o
              | _ -> raise (Pdf.PDFError "Bad or missing /O entry")
            in let u =
              match Pdf.lookup_direct pdf "/U" encryptdict with
              | Some (Pdf.String u) -> u
              | _ -> raise (Pdf.PDFError "Bad or missing /U entry")
            in let p =
              match Pdf.lookup_direct pdf "/P" encryptdict with
              | Some (Pdf.Integer flags) -> i32ofi flags
              | _ -> raise (Pdf.PDFError "Bad or missing /P entry")
            in let id =
              match Pdf.lookup_direct pdf "/ID" pdf.Pdf.trailerdict with
              | Some (Pdf.Array [Pdf.String s; _]) -> s
              | _ -> raise (Pdf.PDFError "Bad or missing /ID element")
            in let oe =
              match Pdf.lookup_direct pdf "/OE" encryptdict with
              | Some (Pdf.String s) -> Some s
              | _ -> None
            in let ue =
              match Pdf.lookup_direct pdf "/UE" encryptdict with
              | Some (Pdf.String s) -> Some s
              | _ -> None
            in
              (*Printf.printf "Encryption Values...\n";
              Printf.printf "crypt_type = %s\n" (string_of_encryption crypt_type);
              Printf.printf "p = %li\n" p;
              Printf.printf "u = %s\n" (printable_of_string u);
              Printf.printf "o = %s\n" (printable_of_string o);
              if ue <> None then Printf.printf "ue = %s\n" (printable_of_string (unopt ue));
              if oe <> None then Printf.printf "oe = %s\n" (printable_of_string (unopt oe));*)
              crypt_type, u, o, p, id, ue, oe

(* Permissions *)
type permission =
  | NoEdit (* R2, Bit 4 *)
  | NoPrint (* R2, Bit 3 *)
  | NoCopy (* R2, Bit 5 *)
  | NoAnnot (* R2, Bit 6 *)
  | NoForms (* R3 only, Bit 9 *)
  | NoExtract (* R3 only, Bit 10 *)
  | NoAssemble (* R3 only, Bit 11 *)
  | NoHqPrint (* R3 only, Bit 12 *)

let string_of_permission = function
  | NoEdit -> "NoEdit"
  | NoPrint -> "NoPrint"
  | NoCopy -> "NoCopy"
  | NoAnnot -> "NoAnnot"
  | NoForms -> "NoForms"
  | NoExtract -> "NoExtract"
  | NoAssemble -> "NoAssemble"
  | NoHqPrint -> "NoHqPrint"

let string_of_bans bans =
  fold_left ( ^ ) "" (interleave " " (map string_of_permission bans))
  
let p_of_banlist toban =
  let p = ref 0l in
    let setbit n b =
      if b then p := Int32.logor !p (Int32.shift_left 1l (n - 1))
    in let notin =
      notpred (mem' toban)
    in
      setbit 3 (notin NoPrint);
      setbit 4 (notin NoEdit);
      setbit 5 (notin NoCopy);
      setbit 6 (notin NoAnnot);
      setbit 7 true;
      setbit 8 true;
      setbit 9 (notin NoForms);
      setbit 10 (notin NoExtract);
      setbit 11 (notin NoAssemble);
      setbit 12 (notin NoHqPrint);
      iter (fun x -> setbit x true) (ilist 13 32);
      !p

let banlist_of_p p =
  let l = ref []
  in let bitset n =
    Int32.logand (Int32.shift_right p (n - 1)) 1l = 0l
  in
    if bitset 3 then l =| NoPrint;
    if bitset 4 then l =| NoEdit;
    if bitset 5 then l =| NoCopy;
    if bitset 6 then l =| NoAnnot;
    if bitset 9 then l =| NoForms;
    if bitset 10 then l =| NoExtract;
    if bitset 11 then l =| NoAssemble;
    if bitset 12 then l =| NoHqPrint;
    !l


let print_string name s =
  flprint name;
  iter (Printf.printf "%i ") (map int_of_char (explode s));
  flprint "\n"

(* New r = 6 algorithm (2.B in the standard) *)

(* modulus 3 of the first 16 bytes of a string taken as a 128 bit big-endian
number. Since (xy mod n) is equal to (x mod n + y mod n) and 256 mod 3 is 1, we
can just sum the bytes and take the modulus afterward. Logic due to Jay
Birkenbilt. *)
let mod3 b =
  let x = ref 0 in
    for i = 0 to 15 do x += int_of_char b.[i] done;
    !x mod 3

let prs s =
  String.iter (fun x -> Printf.printf "%02x" (int_of_char x)) (if String.length s > 16 then String.sub s 0 16 else s);
  flprint "\n"

let shamix password udata i =
  if !crypt_debug then
    begin
      flprint "Beginning of shamix\n";
      flprint "Password is\n";
      prs password;
      flprint "udata is\n";
      prs (match udata with None -> "" | Some x -> x);
    end;
  let k = ref (Pdfcryptprimitives.sha256 i)
  and fin = ref false
  and round = ref 0
  and last_e = ref 0 in
    (*flprint "initial hash is\n";
    prs !k;*)
    while not !fin do
      round += 1;
      (*Printf.printf "THIS IS ROUND %i\n" !round;*)
      let k1 = password ^ !k ^ match udata with None -> "" | Some x -> x in
        (*flprint "K1 is\n";
        prs k1;*)
        let k1_64 = String.concat "" (many k1 64) in
          let e =
            let key = int_array_of_string (String.sub !k 0 16) 
            and firstblock = int_array_of_string (String.sub !k 16 16) in
              let raw =
                string_of_bytes (aes_encrypt_data ~firstblock:firstblock 4 key (Pdfio.bytes_of_string k1_64))
              in
                String.sub raw 16 (String.length raw - 32)
          in
            (*flprint "E is:\n";
            prs e;*)
            last_e := int_of_char e.[String.length e - 1];
            k := (match mod3 e with
                    0 -> (*flprint "using sha256\n";*) Pdfcryptprimitives.sha256
                    | 1 -> (*flprint "using sha384\n";*) Pdfcryptprimitives.sha384
                    | _ -> (*flprint "using sha512\n";*) Pdfcryptprimitives.sha512) (Pdfio.input_of_string e);
            (*flprint "New k is:\n";
            prs !k;
      Printf.printf "!last_e = %i, !round - 32 = %i\n" !last_e (!round - 32);*)
      fin := !round >= 64 && !last_e <= !round - 32
    done;
    let result =
    String.sub !k 0 32
    in
      if !crypt_debug then
        begin
          flprint "RESULT:\n";
          prs result
        end;
      result

(* Conversion via unicode and SASLprep required here for complicated passwords. *)
let make_utf8 pw =
  if String.length pw > 127 then String.sub pw 0 127 else pw

let zero_iv = String.make 16 '\000'

(* Part of Algorithm 3.2a - making the intermediate key using owner password. *)
let file_encryption_key_aesv3 ?digest iso utf8pw o oe u =
  if String.length o < 48 || String.length u < 48 then raise (Pdf.PDFError "/O too short in make_intermediate_owner_key_aesv3") else
    let d =
      match digest with
      | Some d -> 
         if Array.length d <> 32 then Printf.printf "file_encryption_key_aesv3 pre-made length %i\n" (Array.length d);
         d
      | None ->
          let i =
            int_array_of_string
            ((if iso then shamix utf8pw (Some u) else Pdfcryptprimitives.sha256)
                (Pdfio.input_of_string (String.concat "" [utf8pw; String.sub o 40 8; String.sub u 0 48])))
          in
            if Array.length i <> 32 then Printf.printf "file_encryption_key_aesv3 made length %i\n" (Array.length i);
            i
    in
      aes_decrypt_data ~remove_padding:false 8 d (bytes_of_string (zero_iv ^ oe))

let file_encryption_key_aesv3_user iso utf8pw u ue =
  if String.length u < 48 then raise (Pdf.PDFError "/U too short in file_encryption_key_aesv3_user") else
    aes_decrypt_data ~remove_padding:false
      8
      (int_array_of_string ((if iso then shamix utf8pw None else Pdfcryptprimitives.sha256) (Pdfio.input_of_string (String.concat "" [utf8pw; String.sub u 40 8]))))
      (bytes_of_string (zero_iv ^ ue))

(* Algorithm 3.12 - Authenticating the owner password. *)
let authenticate_owner_password_aesv3 iso utf8pw u o =
  if String.length o < 48 || String.length u < 48 then raise (Pdf.PDFError "/O too short in authenticate_owner_password") else
    (if iso then shamix utf8pw (Some u) else Pdfcryptprimitives.sha256) (Pdfio.input_of_string (String.concat "" [utf8pw; String.sub o 32 8; String.sub u 0 48])) = String.sub o 0 32

(* Algorithm 3.11 - Authenticating the user password. *)
let authenticate_user_password_aesv3 iso utf8pw u =
  if String.length u < 48 then raise (Pdf.PDFError "/U too short in authenticate_owner_password") else
    (if iso then shamix utf8pw None else Pdfcryptprimitives.sha256) (Pdfio.input_of_string (String.concat "" [utf8pw; String.sub u 32 8])) = String.sub u 0 32

(* Part of algorithm 3.2a - return p from perms so we can check they match *)
let p_of_perms key perms =
  if String.length perms < 16 then raise (Pdf.PDFError "Wrong length in /Perms") else
  let ps = aes_decrypt_data_ecb ~remove_padding:false 8 (int_array_of_bytes key) (bytes_of_string perms) in
    let ints = int_array_of_bytes ps in
      if ints.(9) <> int_of_char 'a' || ints.(10) <> int_of_char 'd' || ints.(11) <> int_of_char 'b'
        then None
        else
          Some
            (lor32
              (lor32 (lsl32 (i32ofi ints.(0)) 0) (lsl32 (i32ofi ints.(1)) 8))
              (lor32 (lsl32 (i32ofi ints.(2)) 16) (lsl32 (i32ofi ints.(3)) 24)))

(* Main function for decryption. *)
let decrypt_pdf ?keyfromowner user_pw pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> Some pdf, []
  | Some encrypt_dict ->
     let crypt_type, u, o, p, id, ue, oe = get_encryption_values pdf in
       let r, keylength, file_encryption_key =
         match crypt_type with
         | AESV2 -> 4, 128, None
         | AESV3 iso ->
             begin match oe, ue with
             | Some _, Some ue ->
                 begin match keyfromowner with
                 | Some k -> 5, 256, Some k
                 | None ->
                     let perms =
                       match Pdf.lookup_direct pdf "/Perms" encrypt_dict with
                       | Some (Pdf.String s) -> s
                       | _ -> raise (Pdf.PDFError "Missing /Perms in encryption dictionary")
                     in
                       let auth = authenticate_user_password_aesv3 iso (make_utf8 user_pw) u in
                         if not auth then 5, 256, None else
                           let key = file_encryption_key_aesv3_user iso (make_utf8 user_pw) u ue in
                             match p_of_perms key perms with
                             | None -> raise (Failure "/Perms file permissions corrupted")
                             | Some x when x = p -> 5, 256, Some (string_of_bytes key)
                             | Some _ -> raise (Failure "Mismatched /Perms and /P permissions")
                 end
             | _ -> raise (Failure "decrypt_pdf: no oe")
             end
         | ARC4 (k, r) -> r, k, None
       in let encrypt_metadata =
         match Pdf.lookup_direct pdf "/EncryptMetadata" encrypt_dict with
         | Some (Pdf.Boolean false) -> false
         | _ -> true
       in
         process_cryption (not encrypt_metadata) false pdf crypt_type user_pw r u o p id keylength file_encryption_key,
         banlist_of_p p

(* Calculate the owner key from the padded owner password (as calculated by
pad_password) *)
let owner_key padded_owner keylength r =
  let digest1 = Digest.string (string_of_int_array padded_owner) in
    let digest2 =
      if r >= 3 then
        let d = ref digest1 in
          for x = 1 to 50 do
            d := Digest.string !d
          done;
          !d
        else
          digest1
    in
      int_array_of_string (String.sub digest2 0 (keylength / 8))

(* Calculate XOR keys *)
let mkkey key x =
  let key' = Array.copy key in
    for k = 0 to Array.length key - 1 do
      key'.(k) <- key.(k) lxor x 
    done;
    key'

(* Just decrypt a single stream, given the user password, and pdf. This is used
to decrypt cross-reference streams during the reading of a file -- the PDF is
only partially formed at this stage. *)
let decrypt_single_stream user_pw owner_pw pdf obj gen stream =
  (*i Printf.printf "decrypt_single_stream\n";
  begin match user_pw with None -> flprint "no user password\n" | Some x -> Printf.printf "user password |%s|\n" x end;
  begin match owner_pw with None -> flprint "no owner password\n" | Some x -> Printf.printf "owner password |%s|\n" x end; i*)
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> stream 
  | Some encrypt_dict ->
     let crypt_type, u, o, p, id, ue, oe = get_encryption_values pdf in
       let r, keylength =
         match crypt_type with
         | AESV2 -> 4, 128
         | AESV3 b -> (if b then 6 else 5), 256
         | ARC4 (k, r) -> r, k
       in let no_encrypt_metadata =
         match Pdf.lookup_direct pdf "/EncryptMetadata" encrypt_dict with
         | Some (Pdf.Boolean false) -> true
         | _ -> false
       in
           match r, keylength, ue, oe with
           | (5 | 6), 256, Some ue, Some oe ->
               let owner_pw = match owner_pw with Some x -> x | None -> ""
               and user_pw = match user_pw with Some x -> x | None -> "" in
                 (* Generate a file encryption key from either the owner or user password *)
                 let file_encryption_key =
                   if authenticate_user_password_aesv3 (r = 6) (make_utf8 user_pw) u
                     then file_encryption_key_aesv3_user (r = 6) (make_utf8 user_pw) u ue
                     else if authenticate_owner_password_aesv3 (r = 6) (make_utf8 owner_pw) u o
                       then file_encryption_key_aesv3 (r = 6) (make_utf8 owner_pw) o oe u
                       else raise (Pdf.PDFError "Encryption: Could not decrypt single stream: Bad AESV3 user or owner password")
                 in
                   decrypt_stream
                     crypt_type pdf no_encrypt_metadata false obj gen (int_array_of_bytes file_encryption_key)
                     keylength r (Some (string_of_bytes file_encryption_key)) stream
           | _ ->
         if owner_pw <> None then
           (* Decode with owner password *)
           let owner_pw_string = unopt owner_pw in
             (* Authenticate the owner password and calculate the key *)
             let padded_owner = pad_password (int_array_of_string owner_pw_string) in
               let key = owner_key padded_owner keylength r in
                 let user_pw =
                   if r = 2 then
                     string_of_bytes (crypt key (bytes_of_string o))
                   else (* r >= 3 *)
                     begin
                       let acc = ref (bytes_of_string o) in
                         for x = 19 downto 0 do
                           acc := crypt (mkkey key x) !acc
                         done;
                         string_of_bytes !acc 
                     end
                 in
                   if authenticate_user no_encrypt_metadata user_pw r u o p id keylength then
                     let key = find_key no_encrypt_metadata user_pw r o p id keylength in
                       decrypt_stream crypt_type pdf no_encrypt_metadata false obj gen key keylength r None stream
                   else
                     raise (Pdf.PDFError "Encryption: Bad owner password when decrypting single stream")
         else
           (* We're using user password. If none, assume it's blank *)
           let user_pw_string = match user_pw with None -> "" | Some x -> x in
             if authenticate_user no_encrypt_metadata user_pw_string r u o p id keylength then
               let key = find_key no_encrypt_metadata user_pw_string r o p id keylength in
                 decrypt_stream crypt_type pdf no_encrypt_metadata false obj gen key keylength r None stream
             else
               raise (Pdf.PDFError "Encryption: Bad password when decrypting single stream")

(* Decrypt with the owner password. *)
let decrypt_pdf_owner owner_pw pdf =
  (*Printf.printf "decrypt_pdf_owner with owner pw A%sA\n" owner_pw; flprint "\n";*)
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> Some pdf
  | _ ->
    let padded_owner = pad_password (int_array_of_string owner_pw) in
      let crypt_type, u, o, _, _, _, oe = get_encryption_values pdf in
        let r, keylength =
          match crypt_type with
          | AESV2 -> 4, 128
          | AESV3 x -> (if x then 6 else 5), 256
          | ARC4 (k, r) -> r, k
        in
          if r = 5 || r = 6 then
            if authenticate_owner_password_aesv3 (r = 6) (make_utf8 owner_pw) u o then
            begin
              match oe with
              | None -> raise (Pdf.PDFError "decrypt_pdf_owner: No /OE entry found")
              | Some oe ->
                  let key = string_of_bytes (file_encryption_key_aesv3 (r = 6) (make_utf8 owner_pw) o oe u) in
                    fst (decrypt_pdf "" ~keyfromowner:key pdf)
            end
            else
                None
          else
            let user_pw =
              let key = owner_key padded_owner keylength r in
                if r = 2 then
                  string_of_bytes (crypt key (bytes_of_string o))
                else (* r >= 3 *)
                  begin
                    let acc = ref (bytes_of_string o) in
                      for x = 19 downto 0 do
                        acc := crypt (mkkey key x) !acc
                      done;
                      string_of_bytes !acc 
                  end
            in
              fst (decrypt_pdf user_pw pdf)

(* Make an owner password *)
let mk_owner r owner_pw user_pw keylength =
  let padded_owner =
    let source =
      if owner_pw = "" then user_pw else owner_pw
    in
     pad_password (int_array_of_string source)
  in
    let key = owner_key padded_owner keylength r in
      let padded_user = pad_password (int_array_of_string user_pw) in
        if r = 2 then
          string_of_bytes (crypt key (bytes_of_int_array padded_user))
        else (* r >= 3 *)
          let acc = ref (crypt key (bytes_of_int_array padded_user)) in
            for x = 1 to 19 do
              acc := crypt (mkkey key x) !acc
            done;
            string_of_bytes !acc
            
(* Make a user password *)
let mk_user no_encrypt_metadata user_pw o p id r keylength =
  let key = find_key no_encrypt_metadata user_pw r o p id keylength in
    if r = 2 then
      string_of_bytes (crypt key (bytes_of_int_array paddings))
    else (* r >= 3 *)
      let digest_input = [paddings; int_array_of_string id] in
        let d = Digest.string (string_of_int_arrays digest_input) in
          let acc = ref (crypt key (bytes_of_string d)) in
            for x = 1 to 19 do
              acc := crypt (mkkey key x) !acc
            done;
            string_of_bytes !acc ^ (implode (many '\000' 16))

(* Get the ID, or add one if there's not one there. Return the updated pdf and
the ID *)
let get_or_add_id pdf =    
  match Pdf.lookup_direct pdf "/ID" pdf.Pdf.trailerdict with
  | Some (Pdf.Array [Pdf.String s; _]) ->
      s, pdf
  | _ ->
      let idobj = Pdf.generate_id pdf "" (fun () -> Random.float 1.) in
        let pdf' =
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/ID" idobj}
        in
          match idobj with
          | Pdf.Array [Pdf.String s; _] -> s, pdf'
          | _ -> assert false

(* 40bit encryption *)
let encrypt_pdf_40bit_inner owner user p user_pw id pdf =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 1;
       "/R", Pdf.Integer 2;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p)]
  in
    match process_cryption false false pdf (ARC4 (40, 2)) user_pw 2 user owner p id 40 None with
    | Some pdf ->
        {pdf with
          Pdf.trailerdict =
            Pdf.add_dict_entry
              pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
    | None -> raise (Pdf.PDFError "Encryption 40 failed")

let encrypt_pdf_40bit user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  in let owner = mk_owner 2 owner_pw user_pw 40
  in let id, pdf = get_or_add_id pdf in
    let user = mk_user false user_pw owner p id 2 40 in
      encrypt_pdf_40bit_inner owner user p user_pw id pdf

(* 128bit encryption *)
let encrypt_pdf_128bit_inner owner user p user_pw id pdf =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 2;
       "/R", Pdf.Integer 3;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/Length", Pdf.Integer 128;
       "/P", Pdf.Integer (i32toi p)]
  in
    match process_cryption false false pdf (ARC4 (128, 3)) user_pw 3 user owner p id 128 None with
    | Some pdf ->
        {pdf with
          Pdf.trailerdict =
            Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
    | None -> raise (Pdf.PDFError "Encryption 128 failed")

let encrypt_pdf_128bit_inner_r4 owner user p user_pw id pdf encrypt_metadata =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 4;
       "/CF",
          Pdf.Dictionary
            ["/StdCF",
              Pdf.Dictionary
                ["/Length", Pdf.Integer 16;
                 "/AuthEvent", Pdf.Name "/DocOpen";
                 "/CFM", Pdf.Name "/V2"]];
       "/EncryptMetadata", Pdf.Boolean encrypt_metadata;
       "/Length", Pdf.Integer 128;
       "/R", Pdf.Integer 4;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p);
       "/StrF", Pdf.Name "/StdCF";
       "/StmF", Pdf.Name "/StdCF"]
  in
    match process_cryption (not encrypt_metadata) false pdf (ARC4 (128, 4)) user_pw 4 user owner p id 128 None with
    | Some pdf ->
        {pdf with
          Pdf.trailerdict =
            Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
    | None -> raise (Pdf.PDFError "Encryption 128 r4 failed")

let encrypt_pdf_128bit user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  in let owner = mk_owner 3 owner_pw user_pw 128
  in let id, pdf = get_or_add_id pdf in
    let user = mk_user false user_pw owner p id 3 128 in
      encrypt_pdf_128bit_inner owner user p user_pw id pdf

(* AES Encryption. *)
let encrypt_pdf_AES_inner owner user p user_pw id encrypt_metadata pdf =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 4;
       "/CF",
          Pdf.Dictionary
            ["/StdCF",
              Pdf.Dictionary
                ["/Length", Pdf.Integer 16;
                 "/AuthEvent", Pdf.Name "/DocOpen";
                 "/CFM", Pdf.Name "/AESV2"]];
       "/EncryptMetadata", Pdf.Boolean encrypt_metadata;
       "/Length", Pdf.Integer 128;
       "/R", Pdf.Integer 4;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p);
       "/StrF", Pdf.Name "/StdCF";
       "/StmF", Pdf.Name "/StdCF"]
  in
    match 
      process_cryption
        (not encrypt_metadata) true pdf AESV2 user_pw 4 user owner p id 128 None
    with
    | Some pdf ->
        {pdf with
          Pdf.trailerdict =
            Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
    | None -> raise (Pdf.PDFError "Encryption AES failed")

let encrypt_pdf_AES encrypt_metadata user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  in let owner = mk_owner 4 owner_pw user_pw 128
  in let id, pdf = get_or_add_id pdf in
    let user = mk_user (not encrypt_metadata) user_pw owner p id 4 128 in
      encrypt_pdf_AES_inner owner user p user_pw id encrypt_metadata pdf

let encrypt_pdf_AES256_inner iso encrypt_metadata owner user p perms oe ue id key pdf =
  if !crypt_debug then Printf.printf "encrypt_pdf_AES256_inner, ISO = %b\n" iso;
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 5;
       "/CF",
          Pdf.Dictionary
            ["/StdCF",
              Pdf.Dictionary
                ["/Length", Pdf.Integer 32;
                 "/AuthEvent", Pdf.Name "/DocOpen";
                 "/CFM", Pdf.Name "/AESV3"]];
       "/EncryptMetadata", Pdf.Boolean encrypt_metadata;
       "/Length", Pdf.Integer 256;
       "/R", Pdf.Integer (if iso then 6 else 5);
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p);
       "/StrF", Pdf.Name "/StdCF";
       "/StmF", Pdf.Name "/StdCF";
       "/Perms", Pdf.String perms;
       "/OE", Pdf.String oe;
       "/UE", Pdf.String ue]
  in
    match
      process_cryption
        (not encrypt_metadata) true pdf (AESV3 iso) "" (if iso then 6 else 5) user owner p id 256 (Some key)
    with
    | Some pdf ->
        {pdf with
          Pdf.trailerdict =
            Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
    | None -> raise (Pdf.PDFError "256 bit Encryption AES failed")

(* Algorithm 3.10 *)
let perms_of_p ?digest iso encrypt_metadata p utf8pw o oe u =
  let extendedp = lor64 0xFFFFFFFF00000000L (i64ofi32 p) in
    let b = Array.make 16 0 in
      for n = 0 to 7 do
        b.(n) <- i64toi (land64 0x00000000000000FFL (lsr64 (land64 (lsl64 0xFFL (n * 8)) extendedp) (n * 8)))
      done;
      b.(8) <- int_of_char (if encrypt_metadata then 'T' else 'F');
      b.(9) <- int_of_char 'a';
      b.(10) <- int_of_char 'd';
      b.(11) <- int_of_char 'b';
      for n = 12 to 15 do b.(n) <- 0 done;
      let key =
        (int_array_of_string (string_of_bytes (file_encryption_key_aesv3 ?digest iso utf8pw o oe u)))
      in
        aes_encrypt_data_ecb 8 key (bytes_of_string (string_of_int_array b))

(* Algorithm 3.8. Returns u, ue. *)
let make_ue iso file_encryption_key user_pw user_validation_salt user_key_salt =
  let hash =
    if iso then shamix user_pw None else Pdfcryptprimitives.sha256
  in
  let u =
    String.concat
      ""
      [(hash (Pdfio.input_of_string (String.concat "" [user_pw; user_validation_salt]))); user_validation_salt; user_key_salt]
  in
    let ue = 
      aes_encrypt_data ~firstblock:(int_array_of_string zero_iv) 8
        (int_array_of_string (hash (Pdfio.input_of_string (String.concat "" [user_pw; user_key_salt]))))
        file_encryption_key
    in
      u, String.sub (string_of_bytes ue) 16 32

(* Algorithm 3.9 *)
let make_oe iso file_encryption_key owner_pw owner_validation_salt owner_key_salt u =
  let hash =
    if iso then shamix owner_pw (Some u) else Pdfcryptprimitives.sha256
  in
    let o =
      String.concat
       ""
       [(hash (Pdfio.input_of_string (String.concat "" [owner_pw; owner_validation_salt; u]))); owner_validation_salt; owner_key_salt]
    in
      let digest =
        int_array_of_string (hash (Pdfio.input_of_string (String.concat "" [owner_pw; owner_key_salt; u])))
      in
        let oe = aes_encrypt_data ~firstblock:(int_array_of_string zero_iv) 8 digest file_encryption_key in
          o, String.sub (string_of_bytes oe) 16 32, digest

let mksalt () =
  let s = Array.make 8 0 in
  for x = 0 to 7 do s.(x) <- ran255 () done;
    string_of_int_array s

let mkfilekey () =
  let s = Array.make 32 0 in
    for x = 0 to 31 do s.(x) <- ran255 () done;
    string_of_int_array s

let encrypt_pdf_AES256_call iso encrypt_metadata user_pw owner_pw banlist pdf =
  let user_pw = make_utf8 user_pw
  and owner_pw = make_utf8 owner_pw
  and user_validation_salt = mksalt ()
  and user_key_salt = mksalt ()
  and owner_validation_salt = mksalt ()
  and owner_key_salt = mksalt ()
  and file_encryption_key = bytes_of_string (mkfilekey ()) in
    let p = p_of_banlist banlist in
      let u, ue = make_ue iso file_encryption_key user_pw user_validation_salt user_key_salt in
        let o, oe, digest = make_oe iso file_encryption_key owner_pw owner_validation_salt owner_key_salt u in
          let id, pdf = get_or_add_id pdf in
            let perms = perms_of_p ~digest:digest iso encrypt_metadata p owner_pw o oe u in
              encrypt_pdf_AES256_inner
                iso encrypt_metadata o u p (string_of_bytes perms) oe ue id
                (string_of_bytes file_encryption_key) pdf

let encrypt_pdf_AES256 =
  encrypt_pdf_AES256_call false

let encrypt_pdf_AES256ISO =
  encrypt_pdf_AES256_call true

(* Is a file encrypted? *)
let is_encrypted pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | Some _ -> true
  | None -> false

(* recrypt_pdf original decrypted_and_modified user_password_used
re-encrypts a PDF document which was decrypted with the user password given
using that same user password, the owner password from the original encrypted
file and the same permissions and encryption parameters. **modified is nuked**. *)
let recrypt_pdf original modified user_pw =
  match Pdf.lookup_direct original "/Encrypt" original.Pdf.trailerdict with
  | None -> raise (Pdf.PDFError "recrypt_pdf: original PDF not encrypted.")
  | Some encrypt_dict ->
     let modified = Pdf.renumber (Pdf.changes modified) modified in
       let crypt_type, u, o, p, id, ue, oe = get_encryption_values original in
         let encrypt_metadata =
           match Pdf.lookup_direct original "/EncryptMetadata" encrypt_dict with
           | Some (Pdf.Boolean false) -> false
           | _ -> true
         in
           match crypt_type with
           | AESV3 iso ->
               let oe, ue =
                 begin match oe with Some oe -> oe | None -> raise (Pdf.PDFError "recrypt_pdf: bad /oe") end,
                 begin match ue with Some ue -> ue | None -> raise (Pdf.PDFError "recrypt_pdf: bad /ue") end
               in
                 let key =
                   if authenticate_user_password_aesv3 iso (make_utf8 user_pw) u
                     then file_encryption_key_aesv3_user iso (make_utf8 user_pw) u ue
                     else raise (Pdf.PDFError "recrypt_pdf: could not build AESV3 file encryption key.")
               in
                 encrypt_pdf_AES256_inner
                   iso encrypt_metadata o u p (string_of_bytes (perms_of_p iso encrypt_metadata p user_pw o oe u))
                   oe ue id (string_of_bytes key) modified
           | AESV2 -> encrypt_pdf_AES_inner o u p user_pw id encrypt_metadata modified
           | ARC4 (40, _) -> encrypt_pdf_40bit_inner o u p user_pw id modified
           | ARC4 (128, 4) -> encrypt_pdf_128bit_inner_r4 o u p user_pw id modified encrypt_metadata
           | ARC4 (128, _) -> encrypt_pdf_128bit_inner o u p user_pw id modified
           | _ -> raise (Pdf.PDFError "recrypt_pdf: bad encryption")

