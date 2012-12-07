queue
=====

PLT Games submission (December 2012): Language based on queue automata


Building
--------

This submission is written in Haskell and built using cabal. To build, perform
the following steps:

```
$ ./Setup.lhs configure --user
$ ./Setup.lhs build
$ dist/build/queue/queue WORD AUTOMATON
```

Running the examples
--------------------

```
$ dist/build/queue/queue 011 examples/1
Accepted.
$ dist/build/queue/queue 01 examples/1
Not accepted.
$ dist/build/queue/queue --separator= 00 examples/1
Suffix found. Accepted word is 00111.
```
