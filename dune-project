(lang dune 3.10)

(generate_opam_files true)

(authors "Vikram Shivakumar" "Nate Brown" "Jacopo Teneggi")

(maintainers "vshivak1@jhu.edu" "nbrown99@jhu.edu" "jtenegg1@jhu.edu")

(package
 (allow_empty)
 (name ocaml_bigbwt)
 (synopsis "An OCaml library for the Burrows-Wheeler Transform.")
 (description
  "This package provides an OCaml implementation of the Prefix-Free Parsing algorithm for the construction of the Burrows-Wheeler Transform for large strings. It provides efficient serching of patterns by means of the FM-Index.")
 (depends
  (ounit2
   (>= 2.2.7))
  (core
   (>= 0.16.2))
  (bitv
   (>= 1.6.0))))
