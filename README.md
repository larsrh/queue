queue
=====

PLT Games submission (December 2012): Language based on queue automata

What it does
------------

`queue` simulates _queue automata_. A queue automaton is a kind of a finite state machine which uses a FIFO queue as memory. Its main component is a table which describes how a certain piece of input transforms the state and the underlying queue. In normal mode of operation, `queue` reads a program, runs it on a given input and prints whether the program accepted the input or not. Strictly speaking, a given program gives rise to a set of accepted inputs (a _language_), and `queue` determines whether the input is an element of that set.

If you want to how a queue automaton is formally defined, check the appropriate [Wikipedia page](http://en.wikipedia.org/wiki/Queue_automaton). In this implementation, δ is non-deterministic and can be partial (hence the sets Σ, Γ and _Q_ don't have to be specified explicitly) and the initial state and queue symbol are determined from the first rule of the program.


Input format
------------

A rule is of the shape `queueHead, inputChar, state -> [push1, push2, ...], state'`. For example, the rule

```
A, 0, q -> [A, A], q
```

triggers when the first symbol in the queue is `A`, a `0` is read from the input, and the automaton is in state `q`. It consumes the input character, removes the head of the queue, appends `A` twice to the queue and stays in state `q`.

How it works
------------

Let's consider a small example. Here's the program:

```
A, 0, q -> [B, A], q
B, 0, q -> [A], q
A, 1, q -> [A], r
```

This describes a very simple automaton with two states (`q` and `r`). First, `queue` determines the start state and the initial queue symbol by looking at the first rule (don't worry, if your program is empty, it is invalid and `queue` will tell you that). Hence, we will start with state `q` and the queue `A`.

Now, we would like to know whether the input word `001` is accepted by that automaton. We do that by consuming its characters one-by-one, filtering the program for matching rule heads, applying the first rule (or stop if no rule matches), and repeating that process.

In our example, the first character is `0`, the active state is `q` and the head of the queue is `A`. Only the first rule matches. Applying it does not change the state, but removes the `A` from the queue and appends `BA` to the queue. The queue now contains `BA`. In the second step, we read the character `0`. Here, the second rule matches, and the queue is now `AA`. The third character is `1`, and the third rule matches, shifting the state to `r` and leaving `AA` as queue content. The input is now exhausted, but the queue still contains characters. Thus, by definition of queue automata, the input is _not accepted_. This is exactly what `queue` will tell us:

```
$ dist/build/queue/queue 001 examples/2
Not accepted.
```

However, we can add just one rule and have our modified program accept the input `001`. `queue` allows the input character of a rule to be empty, that is, it _always_ matches, regardless of whether there is still input to be consumed or not. We express that by just leaving the second field empty:

```
A, , r -> [], r
```

Informally speaking, this means that if the automaton is in state `r`, it can just remove an `A` from the queue at any time. Repeat twice, and we arrive at the empty queue, which means _accept_:

```
$ dist/build/queue/queue 001 examples/3
Accepted.
```

Step modes
----------

Above, I told you that the first matching rule is selected. In fact, there are several modes available:

* `deterministic` chooses the first matching rule
* `all` chooses _all_ possible rules and accepts if _any_ path accepts
* `random` chooses a random rule from the set of possible rules

Computing things
----------------

So far, you only know how to _decide_ things. But `queue` also _computes_ things. The implementation of that is totally naïve: You specify an input word as usual and a separator (e.g. `#` or the empty word), and `queue` will try to append characters to the input until one of those concatenated words is accepted.

For example, running `queue --separator=- 00` will try, in order,

* `00-`,
* `00-0`,
* `00-1`,
* `00-00`,
* `00-01`,

and so on. The set of valid characters for the suffix is the set of all characters occuring in the rules. Make sure that you have rules for the characters appearing in the separator.

Building
--------

This submission is written in Haskell and built using cabal. To build, perform the following steps:

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
