open Core_kernel

module Make (D : sig
  include Comparable.S

  val to_string : t -> string
end) (B : sig
  val bottom_elems : Set.M(D).t
end)
(L : Sig.S) : sig
  include Sig.S

  val set : t -> D.t -> L.t -> t

  val get : t -> D.t -> L.t option
end
