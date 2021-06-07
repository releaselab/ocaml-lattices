open Core_kernel
open QCheck

module L = Flat.Make (struct
  include Bool

  let gen = Gen.bool

  let name = "taint"
end)

module LTests = Lcheck.GenericTopTests (L)

let () = exit (QCheck_base_runner.run_tests LTests.suite)
