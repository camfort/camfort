!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.9   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
MODULE CB
  REAL, unit(ro) :: R0
  REAL, unit(ee) :: EE2
  REAL, unit(ee/ro) :: A0
END MODULE CB
!
PROGRAM BOND
!
! Main program to calculate the bond length of NaCl.
! Copyright (c) Tao Pang 1997.
!
  USE CB
  IMPLICIT NONE
  INTEGER :: ISTEP
  REAL :: DL,X0,DX
!
  A0 = 1090.0
  R0 = 0.33
  EE2 = 14.4
  DL = 1.0E-06
  X0 = 2.0
  DX = 0.1
  CALL M_SECANT (DL,X0,DX,ISTEP)
  WRITE (6,"(I4,2F16.8)") ISTEP,X0,DX
END PROGRAM BOND
!
SUBROUTINE M_SECANT (DL,X0,DX,ISTEP)
!
! Subroutine for the root of f(x) = dg(x)/dx = 0 with the
! secant method with the search toward the maximum of g(x).
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (OUT) :: ISTEP
  REAL, INTENT (IN) :: DL
  REAL, INTENT (INOUT) :: X0,DX
  REAL :: G0,G1,G2,X1,X2,D,G,GX,FX
  real, unit(f) :: F
!
  ISTEP = 0
  G0 = GX(X0)
  X1 = X0+DX
  G1 = GX(X1)
  IF(G1.LT.G0) X1 = X0-DX
  DO WHILE (ABS(DX).GT.DL)
    D  = FX(X1)-FX(X0)
    DX = -(X1-X0)*FX(X1)/D
    X2 = X1+DX
    G2 = GX(X2)
    IF(G2.LT.G1) X2 = X1-DX
    X0 = X1
    X1 = X2
    G1 = G2
    ISTEP = ISTEP+1
  END DO
  X0 = X2
END SUBROUTINE M_SECANT
!
FUNCTION GX(X) RESULT(G)
  USE CB
  IMPLICIT NONE
  REAL :: X,G
!
  G  = EE2/X-A0*EXP(-X/R0)
END FUNCTION GX
!
FUNCTION FX(X) RESULT(F)
  USE CB
  IMPLICIT NONE
  REAL :: X,F
!
  F  = EE2/(X*X)-A0*EXP(-X/R0)/R0
END FUNCTION FX
