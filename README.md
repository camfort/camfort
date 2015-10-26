# CamFort

## Prerequisites

On a Debian/Ubuntu system

```
   sudo apt-get install ghc alex happy libghc-text-dev libghc-comonad-dev libghc-mtl-dev

   cabal update
   cabal install syz generic-deriving uniplate
```

Also required is the language-fortran package which needs to be built from source and installed.  This contains the lexer and the parser.

## Building

For development purpsoses, you probably want to load CamFort in interactive mode:

```
   ghci Main.hs
 ```

OR compile to the binary format locally:

```
  ghc Main.hs -o camfort -package mtl
```

Alternatively, to install as a library in your Haskell build 

```
  runhaskell Setup.hs configure
  runhaskell Setup.hs build
  runhaskell Setup.hs install
```

OR, for local install (depending how your Haskell install is setup), try to configure
using --user.

```
  runhaskell Setup.hs configure --user
```

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
