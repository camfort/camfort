!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 3.C   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM WAVE
!
! Program for the eigenvalue problem with a combination of the
! bisection method and the Numerov algorithm for u" = -k**2*u
! with boundary conditions u(0)=u(1)=0.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=101
  INTEGER :: I,ISTEP
  REAL :: DL,H,AK,BK,DK,EK,F0,F1
  REAL, DIMENSION (N) :: Q,S,U
!
! Initialization of the problem
!
  DL = 1.0E-06
  H  = 1.0/(N-1)
  AK = 2.0
  BK = 4.0
  DK = 1.0
  EK = AK
  U(1) = 0.0
  U(2) = 0.01
  ISTEP = 0
!
  DO I = 1,N
    S(I) = 0.0
    Q(I) = EK*EK
  END DO
  CALL NMRV (N,H,Q,S,U)
  F0 = U(N)
!
! Bisection method for the root
!
  DO WHILE (ABS(DK).GT.DL)
    EK = (AK+BK)/2.0
    DO I = 1,N
      Q(I) = EK*EK
    END DO
    CALL NMRV (N,H,Q,S,U)
    F1 = U(N)
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
  WRITE (6,"(I4,3F16.8)") ISTEP,EK,DK,F1
END PROGRAM WAVE
!
SUBROUTINE NMRV (N,H,Q,S,U)
!
! The Numerov algorithm for the equation u"(x)+q(x)u(x)=s(x)
! as given in Eqs. (3.77)-(3.80) in the book.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: I
  REAL, INTENT (IN) :: H
  REAL :: G,C0,C1,C2,D,UTMP
  REAL, INTENT (IN), DIMENSION (N) :: Q,S
  REAL, INTENT (INOUT), DIMENSION (N) :: U
!
  G = H*H/12.0
!
  DO I = 2, N-1
    C0 = 1.0+G*((Q(I-1)-Q(I+1))/2.0+Q(I))
    C1 = 2.0-G*(Q(I+1)+Q(I-1)+8.0*Q(I))
    C2 = 1.0+G*((Q(I+1)-Q(I-1))/2.0+Q(I))
    D  = G*(S(I+1)+S(I-1)+10.0*S(I))
    UTMP   = C1*U(I)-C0*U(I-1)+D
    U(I+1) = UTMP/C2
  END DO
END SUBROUTINE NMRV
