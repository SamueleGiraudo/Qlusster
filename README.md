# Qlusster
`[]|_|/`

A programming language to build sounds by granular synthesis.

Copyright (C) 2022--2023 [Samuele Giraudo](https://igm.univ-mlv.fr/~giraudo/) -
`giraudo.samuele@uqam.ca` -


## Quick overview
TODO

See this [introductory video](https://youtu.be/Uv2OmGtPK80) (only in french for the time
being).

This program is linked with [Aclove](https://github.com/SamueleGiraudo/Aclove), a
programming language to pilot Qlusster and write high level musical specifications. See
below.


## First examples
TODO


## Versions
Here is the [changelog](Versions.md) of the different versions.


### Dependencies
The following programs or libraries are needed:

+ `pkg-config`
+ `make`
+ `ocaml` (Version `>= 5.0.0`. An inferior but not too old version may be suitable.)
+ `opam`
+ `ocamlbuild` (Available by `opam install ocamlbuild`.)
+ `ocamlfind` (Available by `opam install ocamlfind`.)
+ `extlib` (Available by `opam install extlib`.)
+ `menhir` (Available by `opam install menhir`.)


### Building
Here are the required steps to build the interpreter `qlusster`:

1. Clone the repository somewhere by running
   `git clone https://github.com/SamueleGiraudo/Qlusster.git`.

2. Install all dependencies (see the section above).

3. Build the project by running `make`.

This creates an executable `qlusster`. The following sections explain how to use it.


## User guide
This [page](Help.md) contains the description of the Qlusster language.

Qlusster program files must have `.qlu` as extension. The main command is

```
./qlusster [--help] [--version] --file PATH [--verbose] [--bunch START LEN] [--write]
[--draw] [--play]
```

where

+ `--help` prints the short help.
+ `--version` prints the version and other information.
+ `--file PATH` sets PATH as the path to the Qlusster program to consider.
+ `--verbose` enables the verbose mode.
+ `--bunch START LEN` specifies the part of the generated signal to consider, with its
  starting time `START` and length `LEN` in seconds.
+ `--write` creates the PCM file specified by the program.
+ `--draw` creates the SVG and PNG files specified by the program.
+ `--play` plays the signal specified by the program.


### Using with Aclove
TODO

The [standard library](Aclove), written in Aclove, contains some useful definitions. it
contains some tools, some synthesizers (trying to mimic some existing ones), and some
effects.


## Theoretical aspects
TODO


### Bibliography
TODO

