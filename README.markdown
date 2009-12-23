# HGom #

An haskell clone of the original [java Gom](http://tom.loria.fr) code
generator.

## Compilation ##

To compile hgom, install 
[cabal-install](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall)
and type the following.

    cabal configure
    cabal build

The `hgom` binary is generated in `dist/build/hgom`.

## Installation ##

    cabal install

## Generate developer documentation ##

By default, cabal generates no documentation since only the executable is
exported. Developers still can generate the modules' documentation as follows.

    cabal haddock --executables \
    --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' \
    --hyperlink-source 

Only exported symbols are documented. Add `--internal` for unexported symbols
documentation.

The documentation index is then `dist/doc/html/hgom/hgom/index.html`.

## Running hgom ##

Run `hgom --help` to get some basic help. 
You can test the behaviour of `hgom` by running it as follows. Some examples
are valid files, other ones demonstrate `hgom` error messages.

    hgom tests/examples/test.gom
    hgom tests/examples/simple.gom
    hgom tests/examples/big.gom
    ...

## Test ##

There is no unit test at the moment, but there are some code coverage and
benchmark scripts.

### Code Coverage ###

Code coverage can be tested as follows.

    cabal clean
    cabal configure
    cabal build --ghc-option -fhpc
    cd tests/coverage
    ./coverage.sh

The documentation is generated in `tests/coverage/html`.

### Benchmark ###

There is some benchmark in `tests/bench` that generates bigger and bigger gom
files and runs `hgom` and `gom` on them, measuring the gom/hgom ratio
concerning the number of generated lines (using
[sloccount](http://www.dwheeler.com/sloccount)) and the elapsed time.

    cd tests/bench
    make

It takes some time. The generated files can be plotted using
[gnuplot](http://www.gnuplot.info) for instance.
