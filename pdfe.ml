(* Errors. *)
type logger = string -> unit

let last = ref ""

let suppress_adjacent_duplicates = ref false

let default s = prerr_string s; flush stderr

let logger = ref default

let log s =
  if !suppress_adjacent_duplicates then
    begin
      if s <> !last then (!logger s; last := s)
    end
  else
    !logger s

let logf a b = log (Printf.sprintf a b)
