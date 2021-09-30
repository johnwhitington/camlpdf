(* compressor <infile> <outfile> [<compress>] substitutes data files specified
   by __DATA:<filename>\n into the <infile> template, writing to <outfile>. The
   data is printed in way which meets OCaml's syntax. It is optionally
   compressed by zlib. *)

let go infile outfile compress = ()

let () =
  match Sys.argv with
  | [|_; infile; outfile|] -> go infile outfile false
  | [|_; infile; outfile; "compress"|] -> go infile outfile true
  | _ -> Printf.eprintf "compressor: unknown command line\n"
