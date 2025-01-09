## 1.3.0 (WIP)
 * Fixed various bugs with cross-module behaviour for units-of-measure features
 * All reports from camfort now produce location information in the form `filename:line:col` for better IDE integration.
 * Updated to work with latest fortran-src

## 1.2.0 (Oct 12, 2022)

* Improve CLI help behaviour
* Update to fortran-src 0.11.0
* Support GHC 9.0, 9.2, 9.4
* Dropped support for GHC 8.10 and older

## 1.1.2 (Oct 09, 2021)

Meta update to provide a release to tie new platform builds to.

* Update internal version string (had been left at 1.0)

## 1.1.1 (Sept 24, 2021)

* Fix Flint interface for Windows
* Update to fortran-src 0.6.0
* Improve COMMON block elimination (array type support)

## 1.1.0 (July 06, 2021)

* Update fortran-src

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
