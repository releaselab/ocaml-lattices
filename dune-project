(lang dune 2.8)

(name lattices)

(version dev)

(generate_opam_files true)

(source
 (uri "git+https://gitlab.com/releaselab/fresco/lattices.git"))

(bug_reports "https://gitlab.com/releaselab/fresco/lattices/issues")

(homepage "https://gitlab.com/releaselab/fresco/lattices")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name lattices)
 (synopsis "A library with various implementations and functors of lattices")
 (depends
  (dune :build)
  core_kernel
  bignum
  ppx_sexp_conv
  ppx_deriving
  ppx_string
  (ppx_inline_test :with-test)
  (qcheck :with-test)
  (lcheck :with-test)))
