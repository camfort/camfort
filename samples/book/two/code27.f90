!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.7   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM NEWTON
!
! This program uses the Newton method to find the root of
! f(x)=exp(x)*ln(x)-x*x=0.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER :: ISTEP
  REAL :: A,B,DL,DX,X0,X1,F,FX,DF,DFX
!
  DL = 1.0E-06
  A  = 1.0
  B  = 2.0
  DX = B-A
  X0 = (A+B)/2.0
  ISTEP = 0
  DO WHILE (ABS(DX).GT.DL)
    X1 = X0-FX(X0)/DFX(X0)
    DX = X1-X0
    X0 = X1
    ISTEP = ISTEP+1
  END DO
  WRITE (6,"(I4,2F16.8)") ISTEP,X0,DX
END PROGRAM NEWTON
!
FUNCTION FX(X) RESULT (F)
  IMPLICIT NONE
  REAL :: F
  REAL, INTENT (IN) :: X
!
  F = EXP(X)*ALOG(X)-X*X
END FUNCTION FX
!
FUNCTION DFX (X) RESULT (DF)
  IMPLICIT NONE
  REAL :: DF
  REAL, INTENT (IN) :: X
!
  DF = EXP(X)*(ALOG(X)+1.0/X)-2.0*X
END FUNCTION DFX
