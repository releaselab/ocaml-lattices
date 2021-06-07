module Make (L1 : Sig.S) (L2 : Sig.S) : sig
  type t = L1.t * L2.t

  include Sig.S with type t := t

  val fst : t -> L1.t

  val snd : t -> L2.t

  val of_pair : L1.t * L2.t -> t
end
