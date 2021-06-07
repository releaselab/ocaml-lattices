open Core_kernel

module type S = sig
  type elt

  module Set : Set.S with type Elt.t := elt

  type t = Set.t

  include Sig.S with type t := t
end

module Make (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) : S with type elt = D.t

module Make_reverse (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) (B : sig
  val bottom : Set.M(D).t
end) : sig
  include S with type elt = D.t

  val top : t
end
