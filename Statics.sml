structure Statics : sig
  exception Error of string
  val check : VSIMPL.program -> unit
end = struct
  structure V = VSIMPL
  exception Error of string
  fun assert msg b = if b then () else raise Error msg

  (*  string set data structure.
   *  look at cmlib/set.sig for how to use it. *)
  structure Vars = StringRedBlackSet

  fun check prog = ()
end