(* Errors. *)
type logger = string -> unit

let default s = prerr_string s; flush stderr

let logger = ref default

let log s = !logger s
