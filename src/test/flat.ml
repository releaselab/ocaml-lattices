open Lattices
open QCheck

module Make (E : sig
  type t

  val gen : t Gen.t

  val equal : t -> t -> bool

  val to_string : t -> string

  val name : string
end) : Lcheck.LATTICE = struct
  include Flat.Make (E)

  let equal x y = leq x y && leq y x

  let name = E.name ^ " lattice"

  let arb_elem =
    let mk_element x = Flat.Element x in
    let gen =
      Gen.(
        frequency
          [ (1, return bottom); (1, return top); (2, map mk_element E.gen) ])
    in
    make gen ~print:to_string

  let arb_elem_le e =
    let rec gen () =
      match e with
      | Flat.Bottom -> Gen.return Flat.Bottom
      | e' ->
          Gen.(arb_elem.gen >>= fun x -> if leq x e' then return x else gen ())
    in
    make (gen ()) ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a
end

module L = Make (Int)
module LTests = Lcheck.GenericTopTests (L)

let () = exit (QCheck_base_runner.run_tests LTests.suite)
