HGom
====

An haskell clone of the original [java Gom](http://tom.loria.fr) code generator.

Compilation
-----------

    cabal configure
    cabal build

The `htom` binary is generated in `dist/build/hgom/`.

Installation
------------

    cabal install

Generate developer documentation
--------------------------------

By default, cabal generates no documentation since only the executable is exported.
Developers still can generate the modules' documentation as follows.

    cabal haddock --executables \
    --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' \
    --hyperlink-source 

Only exported symbols are documented. Add `--internal` for unexported symbols documentation.

The documentation index is then `dist/doc/html/hgom/hgom/index.html`.

Test
----

Run `hgom --help` to get some basic help. There is no unit tests at the moment,
you can test the behaviour of `hgom` by running it as follows.

    hgom tests/examples/test.gom
    hgom tests/examples/simple.gom
    hgom tests/examples/big.gom
    ...

Code Coverage
-------------

Code coverage can be tested as follows.

    cabal clean
    cabal configure -fcoverage
    cabal build
    cd tests/coverage
    ./coverage.sh

The documentation is generated in `tests/coverage/html`.
