exception Error of string

let blit_from_bytes src src_off dst dst_off len =
  for i = 0 to len - 1 do
    Bigarray.Array1.set dst (dst_off + i) (Bytes.get src (src_off + i)) ;
  done

let blit_to_bytes src src_off dst dst_off len =
  for i = 0 to len - 1 do
    Bytes.set dst (dst_off + i) (Bigarray.Array1.get src (src_off + i)) ;
  done

let compress refill flush =
  let w = De.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let refill i =
    let tmp = Bytes.create (De.bigstring_length i) in
    let len = refill tmp in
    blit_from_bytes tmp 0 i 0 len ; len in
  let flush o len =
    let tmp = Bytes.create len in
    blit_to_bytes o 0 tmp 0 len ;
    flush tmp len in
  Zl.Higher.compress ~level:3 ~w ~q ~i ~o ~refill ~flush

let uncompress refill flush =
  let allocate bits = De.make_window ~bits in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let refill i =
    let tmp = Bytes.create (De.bigstring_length i) in
    let len = refill tmp in
    blit_from_bytes tmp 0 i 0 len ; len in
  let flush o len =
    let tmp = Bytes.create len in
    blit_to_bytes o 0 tmp 0 len ;
    flush tmp len in
  match Zl.Higher.uncompress ~allocate ~i ~o ~refill ~flush with
  | Ok () -> ()
  | Error (`Msg err) -> raise (Error err)
