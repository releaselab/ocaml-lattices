open QCheck

module Make (L : sig
  include Lattices.Sig.S

  val gen : t Gen.t

  val name : string
end) : LCheck.LATTICE = struct
  open Lattices
  include With_top.Make (L)

  let equal x y = leq x y && leq y x

  let name = L.name ^ " lattice"

  let arb_elem =
    let mk_element x = `Some x in
    let gen =
      Gen.(
        frequency
          [ (1, return bottom); (1, return top); (2, map mk_element L.gen) ])
    in
    make gen ~print:to_string

  let arb_elem_le e =
    let rec gen () =
      match e with
      | e when equal e bottom -> Gen.return bottom
      | e' ->
          Gen.(arb_elem.gen >>= fun x -> if leq x e' then return x else gen ())
    in
    make (gen ()) ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a
end

module L = Make (Int)
module LTests = LCheck.GenericTopTests (L)

let () = exit (QCheck_base_runner.run_tests LTests.suite)
