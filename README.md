# CamFort
CamFort is a refactoring and verification tool for scientific Fortran programs.
It currently supports Fortran 66, 77, 90, 95 and 2003 (somewhat) with various
legacy extensions.

It is a research project developed in the University of Cambridge and University of
Kent.

## Obtaining
**Recommended install method by OS:**

  * **Windows:** prebuilt bundles (includes dependencies) at
    [Releases](https://github.com/camfort/camfort/releases): download
    ones beginning `camfort-bundle-windows`
  * **Mac:** via Homebrew: `brew install camfort/camfort/camfort`
  * **Linux:** prebuilt binaries at
    [Releases](https://github.com/camfort/camfort/releases). See
    [Wiki: Building](https://github.com/camfort/camfort/wiki/Building#system-specific-guides)
    for how to install library dependencies for your OS.

We provide [prebuilt binaries](https://github.com/camfort/camfort/releases) for
Windows, Mac and Linux. For Windows, we also provide archives that bundle the
CamFort executable together with all of its dependencies.

CamFort is also available through Homebrew
[(formula)](https://github.com/camfort/homebrew-camfort):

    brew install camfort/camfort/camfort

An older (~2019) version of CamFort is available on Docker at
[camfort/camfort](https://cloud.docker.com/u/camfort/repository/docker/camfort/camfort).

A new Docker image for developers (still work-in-progress) is available at
`ghcr.io/camfort/camfort:dev`. Suggested invocation is:

  * podman: `podman run --volume $(pwd):/host --workdir /host
    ghcr.io/camfort/camfort:dev <CamFort arguments>`

## Usage
CamFort is a command-line tool, so invoke it from your favourite shell or
command prompt. [Detailed usage
information](https://github.com/camfort/camfort/wiki) is available on the wiki.

## Building
*(If you want a recent build and don't want to sit through the compilation
process, you may wish to instead check the
[Actions](https://github.com/camfort/camfort/actions) tab and download the
relevant build for your system from a recent workflow run.)*

This section only covers building briefly. See [Wiki:
Building](https://github.com/camfort/camfort/wiki/Building) for full details.

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
  * [Z3](https://github.com/Z3Prover/z3) >= 4.5 (executable)

These should be built for your system, but installation method varies.
System-specific guides are provided on the [wiki](https://github.com/camfort/camfort/wiki). *(Alternatively, you could
check the GitHub Actions workflows.)* On Ubuntu:

    apt install libflint-dev liblapack-dev libopenblas-dev z3

Then `stack build` for Stack, or `cabal build` for Cabal.

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

## Support
### For maintainers
See `doc/maintainers.md`.
