!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.4   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM DERIVATIVES
!
! Main program for derivatives of f(x) = sin(x).  F1: f';
! F2: f";  D1: error in f'; and D2: error in f".
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=101
  INTEGER :: I
  REAL :: PI,H
  REAL, DIMENSION (N) :: X,F,F1,D1,F2,D2
!
  PI = 4.0*ATAN(1.0)
  H  = PI/(2*100)
  DO I = 1, N
    X(I) = H*(I-1)
    F(I) = SIN(X(I))
  END DO
  CALL THREE(N,H,F,F1,F2)
  DO I = 1, N
    D1(I) = F1(I)-COS(X(I))
    D2(I) = F2(I)+SIN(X(I))
    WRITE (6, "(5F10.6)") X(I),F1(I),D1(I),F2(I),D2(I)
  END DO
END PROGRAM DERIVATIVES
!
SUBROUTINE THREE (N,H,FI,F1,F2)
!
! Subroutine for 1st and 2nd order derivatives with the three-point
! formulas. Extrapolations are made at the boundaries.  FI: input
! f(x); H: interval; F1: f'; and F2: f".  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: I
  REAL, INTENT (IN) :: H
  REAL, INTENT (IN), DIMENSION (N) :: FI
  REAL, INTENT (OUT), DIMENSION (N) :: F1,F2
!
! f' and f" from three-point formulas
!
  DO I = 2, N-1
    F1(I) = (FI(I+1)-FI(I-1))/(2.*H)
    F2(I) = (FI(I+1)-2.0*FI(I)+FI(I-1))/(H*H)
  END DO
!
! Linear extrapolation for the boundary points
!
  F1(1) = 2.0*F1(2)-F1(3)
  F1(N) = 2.0*F1(N-1)-F1(N-2)
  F2(1) = 2.0*F2(2)-F2(3)
  F2(N) = 2.0*F2(N-1)-F2(N-2)
END SUBROUTINE THREE
