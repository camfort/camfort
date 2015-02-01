!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.5   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM INTEGRAL
!
! Main program for evaluation of an integral with integrand
! sin(x) in the region of [0,pi/2].  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(n) :: N=9
  INTEGER :: I
  REAL, unit(d) :: PI
  real, unit(h) :: H
  REAL :: S
  REAL, DIMENSION (N) :: X
  REAL, DIMENSION (N), unit(f) :: F
!
  PI = 4.0*ATAN(1.0)
  H  = PI/(2*(N-1))
  DO I = 1, N
    X(I) = H*(I-1)
    F(I) = SIN(X(I))
  END DO
  CALL SIMP (N,H,F,S)
  WRITE (6, "(F16.8)") S
END PROGRAM INTEGRAL
!
SUBROUTINE SIMP (N,H,FI,S)
!
! Subroutine for integration over f(x) with the Simpson rule.  FI:
! integrand f(x); H: interval; S: integral.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: I
  REAL, INTENT (IN) :: H
  REAL :: S0,S1,S2
  REAL, INTENT (OUT) :: S
  REAL, INTENT (IN), DIMENSION (N) :: FI
!
  S  = 0.0
  S0 = 0.0
  S1 = 0.0
  S2 = 0.0
  DO I = 2, N-1, 2
    S1 = S1+FI(I-1)
    S0 = S0+FI(I)
    S2 = S2+FI(I+1)
  END DO
  S = H*(S1+4.0*S0+S2)/3.0
!
! If N is even, add the last slice separately
!
  IF (MOD(N,2).EQ.0) S = S &
     +H*(5.0*FI(N)+8.0*FI(N-1)-FI(N-2))/12.0
END SUBROUTINE SIMP
