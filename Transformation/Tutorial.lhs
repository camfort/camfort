This tutorial provides a walkthrough on how to construct a program analysis and refactoring transformation 
for Fortran using CamFort. The task which we'll cover is fairly simple, but gives a flavour of how to use
the parser and existing analysis and transformation infrastructure and tools.

Task: to create a refactoring for Fortran that replaces 'if-then' blocks, which have only a 'true' branch
with 'if-then-else' blocks where the new 'else' now has some logging to show why the guard was false.
The refactoring will also create an analysis report showing which 'if-then' blocks were refactored.

* 1. Understanding the relevant AST components

The first thing we need to know is: how are 'if-then' and 'if-then-else' blocks represented inside of
CamFort. What is the corresponding AST data constructor? We can find this out by using the existing
'ast' analysis function in CamFort with some small sample programs. Let's create a sample program then
in the director "samples/tutorial" called "test.f90" with the code:

  program test 
    implicit none
     
    integer :: n
    read *, n

    if (n < 3) then 
       print *, "n is small"
    end if

    if (mod(n, 2) == 0) then
       print *, "n is even"
    else
       print *, "n is odd"
    end if

  end program test

Let's check first that this is a valid Fortran program by running it through 'gfortran'.


   cd samples/tutorial
   gfortran test.f90 -o test
   ./test 42
 
Which should print out "n is even". 
Next we are going to run this through CamFort using the 'ast' analysis option. We can look at what functionality
is available in CamFort just by running the binary:

  cd ../..
  ./camfort

This show us the various refactoring and analysis options. In this case, we want the "asts" option, and we don't
 need to specify an output directory or any exluded files so we just using the command:

  ./camfort asts samples/tutorial 



