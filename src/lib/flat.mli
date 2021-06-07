type 'a flat = Bottom | Top | Element of 'a

module Make (X : sig
  type t

  val to_string : t -> string

  val equal : t -> t -> bool
end) : sig
  type t = X.t flat

  include Sig.S with type t := t

  val top : t

  val meet : t -> t -> t
end
