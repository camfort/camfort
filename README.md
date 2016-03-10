# CamFort

## Installation & Building

### Using Stack

We recommend using Stack (http://www.haskellstack.org) to build Camfort. 

 1. Install Stack following the instructions here: http://docs.haskellstack.org/en/stable/README/#how-to-install
 2. Checkout the latest version of language-fortran
 3. Checkout the latest version of camfort
 4. Build using Stack

```
git clone git@github.com:dorchard/language-fortran.git
git clone git@github.com:dorchard/camfort.git
cd camfort
stack build
stack install      # install binary
stack exec camfort # run camfort
```

Please note that at this time we have been unable to compile Camfort on Windows due to a problem with the hmatrix dependency. 

### Using Cabal

Cabal does not automatically install the build tools. If you wonder why
checkout haskell/cabal#220.

```
$ cabal install alex happy
```

Install the native packages needed for `hmatrix` dependency

```
sudo apt-get install libgsl0-dev liblapack-dev libatlas-base-dev
```

### For general usage

```
$ cabal install camfort
```

### For development

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal exec bash
```

This spawns a new shell with only the packages within the sandbox available.

```
$ ghc Main.hs -o camfort
```

This generates a camfort executable in the root directory using only the
packages in the sandbox and none of ~/.cabal or system Haskell packages.

## Usage

```
CamFort - Cambridge Fortran Infrastructure.
Usage: camfort <MODE> <INPUT> [OUTPUT] [OPTIONS...]
Refactor functions:
	common         	 [common block elimination]
	commonArg      	 [common block elimination (to parameter passing)]
	equivalence    	 [equivalence elimination]
	dataType       	 [derived data type introduction]
	dead           	 [dead-code elimination]
	units          	 [unit-of-measure inference]
	removeUnits    	 [unit-of-measure removal]

Analysis functions:
	asts           	 [blank analysis, outputs analysis files with AST information]
	lva            	 [live-variable analysis]
	loops          	 [loop information]
	count          	 [count variable declarations]
	criticalUnits  	 [calculate the critical variables for units-of-measure inference]
	ast            	 [print the raw AST -- for development purposes]

Options:
  -v, -?    --version            show version number
  -e FILES  --exclude=FILES      files to exclude (comma separated list, no spaces)
  -s ID     --units-solver=ID    units-of-measure solver. ID = Custom or LAPACK
  -l ID     --units-literals=ID  units-of-measure literals mode. ID = Unitless, Poly, or Mixed
```
