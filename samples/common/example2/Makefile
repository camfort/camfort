help:
	@echo "compile-original\tcompile the original sources"
	@echo "refactor\t\tapply common block to module refactoring"
	@echo "compile-refactor\tcompile the refactored sources"
	@echo "introduce-bug\t\trewrite source code to change real to integer in ex2a.f90"
	@echo "clean\t\t\trestore to original sources"

compile-original:
	gfortran -o ex2 ex2a.f90 ex2b.f90

refactor:
	camfort common ex2a.f90 refactor-ex2a.f90
	@cp shared.f90 shared.from-ex2a
	camfort common ex2b.f90 refactor-ex2b.f90
	@cp shared.f90 shared.from-ex2b
	@diff shared.from-ex2a shared.from-ex2b || echo "*** WARNING! common block mismatch ***"

compile-refactor:
	gfortran -c shared.f90
	gfortran -o ex2 refactor-ex2a.f90 refactor-ex2b.f90 shared.o

introduce-bug:
	sed -i -e "s/real x(2:10)/integer x(2:10)/" ex2a.f90


clean:
	rm -f ex2 shared.* refactor-* *.mod *~ ex2a.f90-e
	git checkout ex2a.f90 ex2b.f90
