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

    hgom examples/simple.gom
    hgom examples/big.gom
    hgom examples/many_errors.gom
    ...

## Test ##

The test suite is compiled only if the `test` flag is
set. For instance, run

    cabal configure -ftest
    cabal build
    ./dist/build/hgom/hgom --test "-a n"

or

    cabal install -ftest
    hgom --test "-a n"

where `n` is the number of generated random inputs for each test case.

For other arguments to the `--test` option, try `hgom --test "--help"`.

### Code Coverage ###

Code coverage can be tested as follows.

    cabal build --ghc-options "-fhpc -fforce-recomp"
    cd test/coverage
    ./coverage.sh

The documentation is generated in `test/coverage/html`.

### Benchmark ###

There is some benchmark in `test/bench` that generates bigger and bigger gom
files and runs `hgom` and `gom` on them, measuring the gom/hgom ratio
concerning the number of generated lines (using
[sloccount](http://www.dwheeler.com/sloccount)) and the elapsed time.

    cd test/bench
    make

It takes some time. The generated files can be plotted using
[gnuplot](http://www.gnuplot.info) for instance.

## Differences with gom ##

### Better ###

 * faster !
 * smaller code, compiles much faster
 * almost 100% code coverage,
 * consistency checks using QuickCheck
 * more things optional: visitable, checker, ...
 * `toHaskell`, `makeRandom`, `depth` and `size`
    methods generation (optional)
 * smaller code for some generated 
   methods (string escaping factorized for instance)
 * faster code for some generated 
   methods (less function calls, more constants)
 * faster parser (`from*`) methods: don't use an
   intermediate ATerm representation
 * pretty-printed generated code, 
   optional compact (no indentation) option

### Worse ###

 * no hooks !
 * no ant task
 * generates no comments

### Different ###

 * slightly different command-line arguments syntax
