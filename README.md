# A simple KenKen solver in GNU Prolog and SWI-Prolog

This is a simple solver for the [KenKen](https://en.wikipedia.org/wiki/KenKen)
puzzle written in Prolog. KenKen is a puzzle similar to Sudoku, but additionally
has arithmetic constraints.

A sample KenKen problem might look like this (example from Wikipedia):

![Sample KenKen Problem](https://upload.wikimedia.org/wikipedia/commons/f/fd/KenKenProblem.svg)

The objective is still to place numbers in a grid so that all numbers in each
row and column are distinct, but additionally they need to satisfy arithmetic
constraints indicated by cages with an operator and a number. This makes the
problem a perfect fit for CLP(FD) capabilities in modern Prolog.

The code is written for both GNU Prolog and SWI-Prolog. In general, SWI-Prolog
is much better than GNU Prolog, but in some circles GNU Prolog may be preferred.
If you are using GNU Prolog and have large puzzles, you might need to fiddle
with its ["vector
max"](http://www.gprolog.org/manual/gprolog.html#fd-set-vector-max%2F1).

The code contains a few simple test cases that follow; that should be enough to
figure out how to let it solve new puzzles.

## License

Public domain.
