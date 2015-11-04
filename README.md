# CamFort

## Installation & Building

### Dependencies

Cabal does not automatically install the build tools. If you wonder why
checkout haskell/cabal#220.

```
$ cabal install alex happy
```

### For general usage

```
$ cabal install camfort
```

### For development

```
$ cabal sandbox init
$ cabal install camfort.cabal --only-dependencies
$ cabal exec bash
```

This spawns a new shell with only the packages within the sandbox available.

```
$ ghc Main.hs -o camfort
```

This generates a camfort executable in the root directory using only the
packages in the sandbox and none of ~/.cabal or system packages.

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
