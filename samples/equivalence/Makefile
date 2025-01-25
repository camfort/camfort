.PHONY: example1 example2

all: example1 example2

example1: example1.ref example1.camfort
	bash -c "diff <(./example1.ref) <(./example1.camfort)"

example1.ref: example1.f90
	gfortran -ffree-line-length-none $< -o $@
 
example1.camfort.f90: example1.f90
	../../docker/camfort-docker.sh equivalence $< $@
 
example1.camfort: example1.camfort.f90
	gfortran -ffree-line-length-none $< -o $@

example2: example2.ref example2.camfort example2.expected
	bash -c "diff <(./example2.ref) <(./example2.expected)"
	bash -c "diff <(./example2.ref) <(./example2.camfort)"

example2.ref: example2.f90
	gfortran -ffree-line-length-none $< -o $@

example2.camfort.f90: example2.f90
	../../docker/camfort-docker.sh equivalence $< $@

example2.camfort: example2.camfort.f90
	gfortran -ffree-line-length-none $< -o $@

example2.expected: example2.expected.f90
	gfortran -ffree-line-length-none $< -o $@

clean:
	rm -rf example2.ref example2.camfort example2.camfort.f90 example2.expected
