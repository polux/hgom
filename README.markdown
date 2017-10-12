# HGom #

An haskell clone of the original [java Gom](http://tom.loria.fr) code
generator.

## Compilation ##

To compile hgom, install [stack](https://www.haskellstack.org/). Then run:

    stack build

The `hgom` binary is generated in a directory displayed by stack. You can run
it from there. Alternatively you can run it using `stack exec hgom`.

## Installation ##

    stack install

## Running hgom ##

Run `hgom --help` to get some basic help. 
You can test the behaviour of `hgom` by running it as follows. Some examples
are valid files, other ones demonstrate `hgom` error messages.

    hgom examples/simple.gom
    hgom examples/big.gom
    hgom examples/many_errors.gom
    ...

## Test ##

Run the tests with:

    stack test --test-arguments "-a N"

where `N` is the number of generated random inputs for each test case.

For a list of all possible test arguments check 
`stack test --test-arguments "--test"`.

### Benchmark ###

There is some benchmark in `test/bench` that generates bigger and bigger gom
files and runs `hgom` and `gom` on them, measuring the gom/hgom ratio
concerning the number of generated lines (using
[sloccount](http://www.dwheeler.com/sloccount)) and the elapsed time.

    cd bench
    make

It takes some time. The generated files can be plotted using
[gnuplot](http://www.gnuplot.info) for instance.

## Differences with gom ##

### Better ###

 * faster!
 * a far less permissive checker
 * smaller code, compiles much faster
 * almost 100% code coverage,
 * unit tests using QuickCheck, both on compiler 
   data structures and generated code
 * regression tests for parser and checker
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

 * no hooks
 * no ant task
 * imports only builtins
 * generates no comments

### Different ###

 * slightly different command-line arguments syntax
