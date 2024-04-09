FROM ocaml/opam:alpine AS init-opam

RUN set -x && \
  # "Update and upgrade default packages" 
  sudo apk update && sudo apk upgrade && \
  sudo apk add gmp-dev

# --- #

FROM init-opam AS build
WORKDIR /app

COPY ./ .

RUN set -x && \
  # Install related pacakges
  opam install . --deps-only 

RUN eval $(opam env) && \
  # Build applications
  dune build --release && \
  sudo cp ./_build/default/bin/main.exe /usr/bin/main.exe

# --- #

FROM alpine:latest AS ocaml-app
WORKDIR /app

COPY --from=build /usr/bin/main.exe .
RUN set -x && \
  # Update and upgrade default packages 
  apk update && apk upgrade && \
  apk add gmp-dev

ENTRYPOINT ["/app/main.exe"]
