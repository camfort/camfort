!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.8   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM ROOT
!
! Main program to use the Secant Method to find the root of
! f(x)=exp(x)*ln(x)-x*x=0.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER :: ISTEP
  REAL :: A,B,DL,DX,X0
!
  DL = 1.0E-06
  A  = 1.0
  B  = 2.0
  DX = (B-A)/10.0
  X0 = (A+B)/2.0
  CALL SECANT (DL,X0,DX,ISTEP)

  WRITE (6,"(I4,2F16.8)") ISTEP,X0,DX
END PROGRAM ROOT
!
SUBROUTINE SECANT (DL,X0,DX,ISTEP)
!
! Subroutine for the root of f(x)=0 with the secant method.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (INOUT) :: ISTEP
  REAL, INTENT (INOUT) :: X0,DX
  REAL :: X1,X2,D,F,FX
  REAL, INTENT (IN) :: DL
!
  ISTEP = 0
  X1 = X0+DX
  DO WHILE (ABS(DX).GT.DL)
    D  = FX(X1)-FX(X0)
    X2 = X1-FX(X1)*(X1-X0)/D
    X0 = X1
    X1 = X2
    DX = X1-X0
    ISTEP = ISTEP+1
  END DO
END SUBROUTINE SECANT
!
FUNCTION FX(X) RESULT (F)
  IMPLICIT NONE
  REAL :: F
  REAL, INTENT (IN) :: X
!
  F = EXP(X)*ALOG(X)-X*X
END FUNCTION FX
