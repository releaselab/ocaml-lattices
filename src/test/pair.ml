open QCheck

module L : Lcheck.LATTICE_TOPLESS = struct
  module L = Int
  open Lattices
  include Pair.Make (L) (L)

  let bot = bottom

  let equal x y = leq x y && leq y x

  let name = "pair lattice"

  let arb_elem =
    let gen = Gen.(pair L.gen L.gen) in
    make gen ~print:to_string

  let arb_elem_le e =
    let rec gen () =
      match e with
      | e when equal e bot -> Gen.return bottom
      | e_1, e_2 ->
          Gen.(
            pair L.gen L.gen >>= fun (x, y) ->
            if L.leq x e_1 && L.leq e_1 x then
              if L.leq y e_2 then return (x, y) else gen ()
            else if L.leq x e_1 then return (x, y)
            else gen ())
    in
    make (gen ()) ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a
end

module LTests = Lcheck.GenericTests (L)

let () = exit (QCheck_base_runner.run_tests LTests.suite)
