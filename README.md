# CamlChat - 1-on-1 Chat CLI app

Simple TCP chat app written with LWT for concurrency.
Usage is as follows: 
```
[ --client ] with optional ipv4 address
[ --file ] with a path value (only available in client mode)
[ --server ] with optional port
[ --help ] prints usage
```

The app can be run with: `opam install . --deps-only && dune exec ChatApp --release -- {args}`

 
