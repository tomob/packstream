(lang dune 2.8)
(name packstream)
(generate_opam_files true)

(maintainers "Tomasz Baranski <tomasz.baranski@posteo.net>")
(authors "Tomasz Baranski")
(version 0.1)
(license BSD-3-Clause)
(source (github tomob/packstream))

(package
 (name packstream)
 (synopsis "Packstream parses and serializes Packstream binary format")
 (description "Packstream parses and serializes Packstream binary format.")
 (depends
   dune
   (ocaml (>= 4.04.1))
   (ounit2 :with-test)
   (core (>= 0.14.0))
   (bitstring (>= 4.1.0))
   (ppx_bitstring (>= 4.1.0))
   (ppx_deriving (>= 5.0.0))))
