open Base
open QCheck

module L = Flat.Make (struct
  include Bool

  let gen = Gen.bool

  let name = "taint"
end)

module LTests = LCheck.GenericTopTests (L)

let () = Caml.exit (QCheck_base_runner.run_tests LTests.suite)
