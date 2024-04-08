(lang dune 3.14)

(name ChatApp)

(generate_opam_files true)

(authors "Fay Carsons")

(package
 (name ChatApp)
 (synopsis "Simple one-on-one socket chat app")
 (depends 
   (ocaml (>= "5.1.1")) 
   dune 
   (lwt (>= "5.7.0")) 
   (ounit2 (>= "2.2.7")) 
   (ounit2-lwt (>= "2.2.7")) 
   (uri (>= "4.4.0"))
   (base (>= "0.16.3"))
   (lwt_ppx (>= "2.1.0")))
 (tags
  (topics TCP CLI Network)))
