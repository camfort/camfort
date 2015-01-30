!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 3.4   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM S_L_LEGENDRE
!
! Main program for solving the Legendre equation with the simplest 
! algorithm for the Sturm-Liouville equation and the bisection method
! for the root search.  Copyright (c) Tao Pang 1997.
! 
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=501
  INTEGER :: ISTEP
  REAL :: DL,H,AK,BK,DK,EK,F0,F1
  REAL, DIMENSION (N) :: U
!
! Initialization of the problem
!
  DL = 1.0E-06
  H  = 2.0/(N-1)
  AK = 0.5
  BK = 1.5
  DK = 0.5
  EK = AK
  U(1)  = -1.0
  U(2)  = -1.0+H
  ISTEP =  0
  CALL SMPL (N,H,EK,U)
  F0 = U(N)-1.0
!
! Bisection method for the root
!
  DO WHILE (ABS(DK).GT.DL)
    EK = (AK+BK)/2.0
    CALL SMPL (N,H,EK,U)
    F1 = U(N)-1.0
    IF ((F0*F1).LT.0) THEN
      BK = EK
      DK = BK-AK
    ELSE
      AK = EK
      DK = BK-AK
      F0 = F1
    END IF
    ISTEP = ISTEP+1
  END DO
!
  WRITE (6,"(I4,3F16.8)") ISTEP,EK,DK,F1
END PROGRAM S_L_LEGENDRE
!
SUBROUTINE SMPL (N,H,EK,U)
!
! The simplest algorithm for the Sturm-Liouville equation.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: I
  REAL, INTENT (IN) :: H,EK
  REAL :: H2,Q,X,P,P1
  REAL, INTENT (OUT), DIMENSION (N) :: U
!
  H2 = 2.0*H*H
  Q = EK*(1.0+EK)
  DO I = 2, N-1
    X  =  (I-1)*H-1.0
    P  =  2.0*(1.0-X*X)
    P1 = -2.0*X*H
    U(I+1) = ((2.0*P-H2*Q)*U(I)+(P1-P)*U(I-1))/(P1+P)
  END DO
END SUBROUTINE SMPL
