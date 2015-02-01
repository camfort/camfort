!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.10   !!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
  REAL,  unit(b) :: B
  REAL, unit(e) :: E
  REAL, unit(a) :: A
END MODULE CB
!
PROGRAM SCATTERING
!
! This is the main program for the scattering problem.
! Copyright (c) Tao Pang 1997.
!
  USE CB
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(1) :: M=21
  INTEGER, PARAMETER :: N=10001
  INTEGER I,J,ISTEP
  REAL :: DL,B0,DB,DX,X0,X,DX0,F,FX,FB,FBX,G1,G2,RU,RUTH,SI
  REAL, DIMENSION (N) :: FI
  REAL, DIMENSION (M) :: THETA,SIG,SIG1
!
  DL = 1.E-06
  B0 = 0.01
  DB = 0.5
  DX = 0.01
  E  = 1.0
  A  = 100.0
  DO I = 1, M
    B  = B0+(I-1)*DB
!
! Calculate the first term of theta
!
    DO J = 1, N
      X = B+DX*J
      FI(J) = 1.0/(X*X*SQRT(FBX(X)))
    END DO
    CALL SIMP(N,DX,FI,G1)
!
! Find r_m from 1-b*b/(r*r)-U/E=0
!
    X0  = B
    DX0 = DX
    CALL SECANT (DL,X0,DX0,ISTEP)
!
! Calculate the second term of theta
!
    DO J = 1, N
      X = X0+DX*J
      FI(J) = 1.0/(X*X*SQRT(FX(X)))
    END DO
    CALL SIMP (N,DX,FI,G2)
    THETA(I) = 2.0*B*(G1-G2)
    END DO
!
! Calculate d_theta/d_b
!
    CALL THREE (M,DB,THETA,SIG,SIG1)
!
! Put the cross section in log form with the exact result of
! the Coulomb scattering (RUTH)
!
    DO I = M, 1, -1
      B      = B0+(I-1)*DB
      SIG(I) = B/ABS(SIG(I))/SIN(THETA(I))
      RUTH   = 1.0/SIN(THETA(I)/2.0)**4/16.0
      SI     = ALOG(SIG(I))
      RU     = ALOG(RUTH)
      WRITE (6,"(3F16.8)") THETA(I),SI,RU
    END DO
END PROGRAM SCATTERING
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
!
FUNCTION FX(X) RESULT (F)
  USE CB
  IMPLICIT NONE
  REAL :: X,F,U,UX
!
  F = 1.0-B*B/(X*X)-UX(X)/E
END FUNCTION FX
!
FUNCTION FBX(X) RESULT (FB)
  USE CB
  IMPLICIT NONE
  REAL :: X,FB
!
    FB = 1.0-B*B/(X*X)
END FUNCTION FBX
! 
FUNCTION UX(X) RESULT (U)
  USE CB
  IMPLICIT NONE
  REAL :: X,U
!
  U = 1.0/X*EXP(-X/A)
END FUNCTION UX
