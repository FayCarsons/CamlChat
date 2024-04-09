# CamlChat - 1-on-1 Chat CLI app written in OCaml 🐫🐫🐫

Simple TCP chat app written with LWT for concurrency.
Args are:

```shell
[ --client ] with optional ipv4 address (default: 127.0.0.1:8080)
[ --server ] with optional port (default: 8080)
[ --help ] prints usage
```

The app can be run with:
`opam install . --deps-only && dune exec ChatApp --release -- {args}`

Or, if you'd like you can pull the Docker image:
`docker pull faycarsons/camlchat`
and run it
`docker run --rm --it --network host camlchat {args}`
