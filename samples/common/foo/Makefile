help:
	@echo "compile-original\tcompile the original sources"
	@echo "refactor\t\tapply common block to module refactoring"
	@echo "compile-refactor\tcompile the refactored sources"
	@echo "introduce-bug\t\trewrite source code to change real to integer in ex2a.f90"
	@echo "clean\t\t\trestore to original sources"

compile-original:
	gfortran -o foo foo.f90 foo2.f90

refactor:
	camfort common foo.f90 refactor-foo.f90
	@cp Common.f90 Common.from-foo
	camfort common foo2.f90 refactor-foo2.f90
	@cp Common.f90 Common.from-foo2
	@diff Common.from-foo Common.from-foo2 || echo "*** WARNING! common block mismatch ***"

compile-refactor:
	gfortran -c Common.f90
	gfortran -o foo refactor-foo.f90 refactor-foo2.f90 Common.o

introduce-bug:
	sed -i -e "s/integer :: i/real :: i/" foo.f90


clean:
	rm -f foo Common.* refactor-* *.mod *~ foo.f90-e
	git checkout foo.f90 foo2.f90
