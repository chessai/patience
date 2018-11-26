[![Hackage](https://img.shields.io/hackage/v/patience.svg)](https://hackage.haskell.org/package/patience)

# patience

## About
This library implements the "patience diff" algorithm, as well as the patience algorithm for the
longest increasing subsequence problem.

Patience diff computes the difference between two lists, for example the lines of two versions of
a source file. It provides a good balance of performance, nice output for humans, and implementation
simplicity. For more information, see these two blog posts: [alfedenzo](http://alfedenzo.livejournal.com/170301.html), [bramcohen](http://bramcohen.livejournal.com/73318.html)

## Install

Install with `cabal (new-)install patience`.