## 1.0.1 (March 08, 2021)

* Update fortran-src, verifiable-expressions versions
* Require minimum Vinyl 0.9 (`RMap` etc. type classes)
* Support at least GHC 8.6, 8.8, 8.10 (on Linux)
* Fix an issue caused by SBV version update

## 1.0 (August 29, 2019)

* Update to new fortran-src version 0.4.0.
* New command: basic-checks, which runs a series of other checks in Simple.hs.
* Add max-lines threshold for snippets output, currently 5.
* Rename units-compile to units-summarise/summarize.
* Bring over same command-line flexibility to units-compile as 'fortran-src -c', can specify multiple files-or-directories.
* Use ModGraph functionality to allow units-summarise to build dependency graphs and summarise in build-order.
* Search includedir recursively for mod-files, like fortran-src.
* Numerous changes to increase strictness and reduce memory usage: use Pipes, avoid constructing needlessly large graphs.

## 0.906 (June 13, 2019)

* Update to new fortran-src version 0.3.0.
* Add array-check, alloc-check, use-check, fp-check and implicit-none features.
  * A collection of sanity checks on Fortran code looking for common issues, possible problems or potential inefficiencies.
* Introduce a divide-and-conquer methodology for solving units-inference problems, reducing the size of the matrices and taking advantage of SMP where available.
* Treat constant-expressions and parameter variables as literals, for units-inference purposes, making it easier to retrofit units annotations onto existing programs.
* Add 'units-infer --show-ast' feature, which decorates the displayed AST (internal data structure) with units information on each expression.
* Made assorted efficiency improvements to the units solver and other features, especially for large projects with many files.
* Add 'units-check --dump-mod-file' feature that lets you view an 'fsmod' file's contents with regard to units info.
* Allow override of Fortran version used by parser using -F option.

## 0.905 (May 18, 2018)

* Greatly improved units-of-measure support
* Separate verification of modules
* Prototype invariants checking feature
* Implicit-none check on program units
* Fortran 95 support
