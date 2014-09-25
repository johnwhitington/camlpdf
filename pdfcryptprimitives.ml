(* Pdfcrypt primitives, split out *)
open Pdfutil
open Pdfio

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

