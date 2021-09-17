# CamFort

CamFort is a refactoring and verification tool for scientific Fortran programs.
It currently supports Fortran 66, 77, and 90 with various legacy extensions.

It is a research project developed in University of Cambridge and University of Kent.

## Building
For more details, please see the
[installation guide](https://github.com/camfort/camfort/wiki/Installation-Guide)
in the wiki.

### Dependencies
CamFort uses [FLINT](https://www.flintlib.org/) for some numerical operations,
and depends on [hmatrix](https://hackage.haskell.org/package/hmatrix), which
requires has (compile time) dependencies on LAPACK and BLAS. For each of those
libraries, you require the built library and header files (sometimes provided in
a "development" distribution of the library e.g. `libXYZ-dev` on Ubuntu).

On Ubuntu, this should be enough:

    apt install libflint-dev liblapack-dev

On Windows, in an MSYS2 environment:

    pacman -S mingw-w64-x86_64-openblas mingw-w64-x86_64-lapack

Then you have to build FLINT manually.

## Usage
For detailed information please check
[the wiki](https://github.com/camfort/camfort/wiki).

### Shell tab completion
To enable Bash autocompletion for camfort, add
`eval "$(camfort --bash-completion-script=$(which camfort))"` to either your `.bashrc` or `.bash_profile` file.

## Contributing
We appreciate any bugs you encounter and kindly request you to submit it as an
issue.

Pull requests are much appreciated, but please contact us first if it is a
substantial change. Make sure to run the test suite before you submit.

If you have scientific code that you would like us to analyse, we would be happy
to add it to CamFort corpus. This helps us finding useful ways to extend CamFort
as well as ensuring it is robust.
