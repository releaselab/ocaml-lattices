module Make (L : Sig.S) : sig
  type t = L.t list

  include Sig.S with type t := t
end
