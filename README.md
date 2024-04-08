# CamlChat - 1-on-1 Chat CLI app written in OCaml 🐫🐫🐫

Simple TCP chat app written with LWT for concurrency.
Args are:

```shell
[ --client ] with optional ipv4 address
[ --server ] with optional port
[ --file ] with a path value (only available in client mode)
[ --help ] prints usage
```

The app can be run with:
`opam install . --deps-only && dune exec ChatApp --release -- {args}`

## File Mode

This is an option in client mode only, program validates the provided path,
connects to the server, sends the file and closes immediately. Currently, there
is not a way to save the file on the server side but data is not altered in
transit so it is technically possible and may be added if I continue working
on this.
