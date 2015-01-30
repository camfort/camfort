!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.B   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                       !
! Please Note:                                                          !
!                                                                       !
! (1) This computer program is written by Tao Pang in conjunction with  !
!     his book, "An Introduction to Computational Physics," published   !
!     by Cambridge University Press in 1997.                            !
!                                                                       !
! (2) No warranties, express or implied, are made for this program.     !
!                                                                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
PROGRAM MILL
!
! Program to fit the Millikan experimental data to a linear curve
! p(x) = a*x+b directly.  One can find a and b from partial D/partial
! a = 0 and partial D/partial b = 0 with D = sum (p(x_i)-f(x_i))**2.
! The result is a = (c1*c3-c4*n)/(c1**2-c2*n) and b = (c1*c4-c2*c3)
! /(c1**2-c2*n) with n being the number of points, c1 = sum x_i, c2
! = sum x_i**2, c3 = sum f(x_i), and c4 = sum x_i*f(x_i).
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=15
  INTEGER :: I
  REAL :: C1,C2,C3,C4,C,A,B
  REAL, DIMENSION (N) :: X,F
  DATA X /4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0, &
          12.0,13.0,14.0,15.0,16.0,17.0,18.0/
  DATA F /6.558,8.206,9.880,11.50,13.14,14.81,16.40,18.04, &
          19.68,21.32,22.96,24.60,26.24,27.88,29.52/
!
  C1 = 0.0
  C2 = 0.0
  C3 = 0.0
  C4 = 0.0
  DO I = 1, N
    C1 = C1+X(I)
    C2 = C2+X(I)*X(I)
    C3 = C3+F(I)
    C4 = C4+F(I)*X(I)
  END DO
  C = C1*C1-C2*N
  A = (C1*C3-C4*N)/C
  B = (C1*C4-C2*C3)/C
  WRITE (6, "('The fundamental charge is 'F6.4,'+-'F6.4)") A,ABS(B)
END PROGRAM MILL
