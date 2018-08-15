help:
	@echo "compile-original\tcompile the original sources"
	@echo "refactor\t\tapply common block to module refactoring"
	@echo "compile-refactor\tcompile the refactored sources"
	@echo "introduce-bug\t\trewrite source code to change real to integer in ex1a.f90"
	@echo "clean\t\t\trestore to original sources"

compile-original:
	gfortran -o ex1 ex1a.f90 ex1b.f90

refactor:
	camfort common ex1a.f90 refactor-ex1a.f90
	@cp Common.f90 Common.from-ex1a
	camfort common ex1b.f90 refactor-ex1b.f90
	@cp Common.f90 Common.from-ex1b
	@diff Common.from-ex1a Common.from-ex1b || echo "*** WARNING! Common block mismatch ***"

compile-refactor:
	gfortran -c Common.f90
	gfortran -o ex1 refactor-ex1a.f90 refactor-ex1b.f90 Common.o

introduce-bug:
	sed -i -e "s/real x, y/integer x, y/" ex1a.f90


clean:
	rm -f ex1 Common.* refactor-* *.mod *~
	git checkout ex1a.f90 ex1b.f90
