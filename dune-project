(lang dune 3.14)

(name ChatApp)

(generate_opam_files true)

(authors "Fay Carsons")

(package
 (name ChatApp)
 (synopsis "Simple one-on-one socket chat app")
 (depends ocaml dune lwt ounit2 ounit2-lwt uri)
 (tags
  (topics TCP CLI Network)))
