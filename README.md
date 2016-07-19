# CamFort

## Installation & Building

Please see the
[installation guide](https://github.com/camfort/camfort/wiki/Installation-Guide)
in the wiki.

## Usage

```
CamFort 0.802 - Cambridge Fortran Infrastructure.
Usage: camfort <MODE> <INPUT> [OUTPUT] [OPTIONS...]
Refactor functions:
	common         	 [common block elimination] 
	commonArg      	 [common block elimination (to parameter passing)] 
	equivalence    	 [equivalence elimination] 
	dataType       	 [derived data type introduction] 
	dead           	 [dead-code elimination] 

Analysis functions:
	asts           	 [blank analysis, outputs analysis files with AST information] 
	lva            	 [live-variable analysis] 
	loops          	 [loop information] 
	count          	 [count variable declarations] 
	ast            	 [print the raw AST -- for development purposes] 
	stencils-check 	 [stencil spec checking] 
	stencils-infer 	 [stencil spec inference] 
	stencils-synth 	 [stencil spec synthesis] 
	units-suggest  	 [suggest variables to annotate for units-of-measure for maximum coverage] 
	units-check    	 [unit-of-measure checking] 
	units-infer    	 [unit-of-measure inference] 
	units-synth    	 [unit-of-measure synthesise specs.] 

Options:
  -v, -?    --version                    show version number
  -e FILES  --exclude=FILES              files to exclude (comma separated list, no spaces)
  -s ID     --units-solver=ID            units-of-measure solver. ID = Custom or LAPACK
  -l ID     --units-literals=ID          units-of-measure literals mode. ID = Unitless, Poly, or Mixed
  -m ID     --stencil-inference-mode=ID  stencil specification inference mode. ID = Do, Assign, or Both
```

### Troubleshooting
 Sometimes MacOSX has problems with GHC and libiconv. Some of the techniques here are useful: http://blog.omega-prime.co.uk/?p=96
