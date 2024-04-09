# CamlChat - 1-on-1 Chat CLI app written in OCaml 🐫🐫🐫

Simple TCP chat app written with LWT for concurrency.
Args are:

```shell
[ --client ] with optional ipv4 address
[ --server ] with optional port
[ --help ] prints usage
```

The app can be run with:
`opam install . --deps-only && dune exec ChatApp --release -- {args}`
