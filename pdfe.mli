(** Errors. *)

(** The type of loggers. Subject to change. *)
type logger = string -> unit

(** Default error logger, writes the string to stderr and flushes. *)
val default : logger

(** The current error logger, set to [default] upon startup. *)
val logger : logger ref

(** Log a string. *)
val log : logger
