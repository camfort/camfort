# CamFort
CamFort is a refactoring and verification tool for scientific Fortran programs.
It currently supports Fortran 66, 77, 90, 95 and 2003 (somewhat) with various
legacy extensions.

It is a research project developed in University of Cambridge and University of
Kent.

## Installation
We provide [prebuilt binaries](https://github.com/camfort/camfort/releases) for
Windows, Mac and Linux. For Windows, we also provide archives that bundle the
CamFort executable together with all of its dependencies.

CamFort is also available through Homebrew
[(formula)](https://github.com/camfort/homebrew-camfort):

    brew install camfort/camfort/camfort

See the [installation
guide](https://github.com/camfort/camfort/wiki/Installation-Guide) on the wiki
for further information.

## Usage
CamFort is a command-line tool, so invoke it from your favourite shell or
command prompt. [Detailed usage
information](https://github.com/camfort/camfort/wiki) is available on the wiki.

## Building
*(If you want a recent build and don't want to sit through the compilation
process, you may wish to instead check the
[Actions](https://github.com/camfort/camfort/actions) tab and download the
relevant build for your system from a recent workflow run.)*

CamFort supports **GHC 8.4 through GHC 8.10**, and builds with both Stack and
Cabal. We regularly test at least the minimum and maximum supported GHCs.
Releases prior to/newer than those may have issues. (We welcome fixes that would
let us support a wider range of compilers!)

You will likely need **at least 3 GiBs of memory** and some patience to build
CamFort.

CamFort depends on the following foreign libraries:

  * [FLINT](https://www.flintlib.org/) >= 2.5
  * LAPACK
  * BLAS
  * Z3 solver (executable)

These should be built for your system, but installation method varies.
System-specific guides are provided on the wiki. *(Alternatively, you could
check the GitHub Actions workflows.)* On Ubuntu:

    apt install libflint-dev liblapack-dev libopenblas-dev z3

Then for Stack:

    stack build

Or Cabal:

    cabal build

## Usage
For detailed information please check [the
wiki](https://github.com/camfort/camfort/wiki).

### Tab Completion (Bash)
To enable Bash autocompletion for camfort, add the following to your
`.bashrc` or `.bash_profile` file:

    eval "$(camfort --bash-completion-script=$(which camfort))"

## Contributing
We appreciate any bugs you encounter and kindly request you to submit it as an
issue.

Pull requests are much appreciated, but please contact us first if it is a
substantial change. Make sure to run the test suite before you submit.

If you have scientific code that you would like us to analyse, we would be happy
to add it to CamFort corpus. This helps us finding useful ways to extend CamFort
as well as ensuring it is robust.
