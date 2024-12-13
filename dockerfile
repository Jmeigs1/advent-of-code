FROM ocaml/opam

WORKDIR /working

COPY . /working

RUN sudo chown -R opam .

WORKDIR /working/2024
