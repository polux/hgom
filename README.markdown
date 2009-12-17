HGom
====

An haskell clone of the original [java Gom](http://tom.loria.fr) code generator.

Compilation
-----------

    cabal configure
    cabal build

The htom binary is generated in dist/build/hgom/

Installation
------------

    cabal install

Generate developer documentation
--------------------------------

    cabal haddock --executables \
    --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' \
    --hyperlink-source 

(add `--internal` for unexported symbols documentation)

The documentation index is then `dist/doc/html/hgom/hgom/index.html`

Test
----

    hgom tests/examples/test.gom
    hgom tests/examples/simple.gom
    hgom tests/examples/big.gom
    ...

Code Coverage
-------------

    cabal clean
    cabal configure -fcoverage
    cabal build
    cd tests/coverage
    ./coverage.sh

The documentation is generated in `tests/coverage/html`