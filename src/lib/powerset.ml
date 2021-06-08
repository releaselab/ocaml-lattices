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
end) =
struct
  type elt = D.t

  module Set = Set.Make (D)

  type t = Set.t [@@deriving sexp_of]

  let bottom = Set.empty

  let join = Set.union

  let meet = Set.inter

  let leq x y = Set.is_subset x ~of_:y

  let to_string x =
    let f x = [%string "%{x#D};"] in
    [%string "[ %{List.to_string ~f (Set.to_list x)} ]"]
end

module Make_reverse (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) (B : sig
  val bottom : Set.M(D).t
end) =
struct
  type elt = D.t

  let bottom = Set.to_list B.bottom

  module Set = Set.Make (D)

  type t = Set.t [@@deriving sexp_of]

  let bottom = Set.of_list bottom

  let top = Set.empty

  let join = Set.inter

  let meet = Set.union

  let leq x y = Set.equal x y || not (Set.is_subset x ~of_:y)

  let to_string x =
    let f x = [%string "%{x#D};"] in
    [%string "[ %{List.to_string ~f (Set.to_list x)} ]"]
end
