!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.6   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM BISECTION
!
! This program uses the bisection method to find the root of
! f(x)=exp(x)*ln(x)-x*x=0.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER :: ISTEP
  REAL :: A,B,DL,DX,X0,X1,F,FX
!
  DL = 1.0E-06
  A  = 1.0
  B  = 2.0
  DX = B - A
  ISTEP = 0
  DO WHILE (ABS(DX).GT.DL)
    X0 = (A+B)/2.0
    IF ((FX(A)*FX(X0)).LT.0) THEN
      B  = X0
      DX = B-A
    ELSE
      A  = X0
      DX = B-A
    END IF
    ISTEP = ISTEP+1
  END DO
  WRITE (6,"(I4,2F16.8)") ISTEP,X0,DX
END PROGRAM BISECTION
!
FUNCTION FX(X) RESULT (F)
  IMPLICIT NONE
  REAL :: F
  REAL, INTENT (IN) :: X
!
  F = EXP(X)*ALOG(X)-X*X
END FUNCTION FX
