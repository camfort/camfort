!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 3.D   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM SCHR
!
! Main program for solving the eigenvalue problem of the
! one-dimensional Schroedinger equation. 
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=501,M=5,IMAX=100
  INTEGER :: I,IM,NL,NR,ISTEP
  REAL :: DL,H2M,EA,EB,E,DE,XL0,XR0,H,C
  REAL :: XL,XR,FACT,F0,F1,E1,SUM,V,VX
  REAL, DIMENSION (N) :: UL,UR,QL,QR,S
!
  DL    =  1.0E-06
  H2M   =  0.5 
  EA    =  2.4 
  EB    =  2.7
  E     =  EA
  DE    =  0.1
  XL0   = -10.0
  XR0   =  10.0
  H     =  (XR0-XL0)/(N-1)
  C     =  1.0/H2M
  UL(1) =  0.0
  UL(2) =  0.01
  UR(1) =  0.0
  UR(2) =  0.01
!
! Set up the potential q(x) and source s(x)
!
  DO I = 1, N
    XL    = XL0+(I-1)*H
    XR    = XR0-(I-1)*H
    QL(I) = C*(E-VX(XL))
    QR(I) = C*(E-VX(XR))
    S(I)  = 0.0
  END DO
!
! Find the matching point at the right turning point
!
  DO I = 1, N-1
    IF(((QL(I)*QL(I+1)).LE.0).AND.(QL(I).GT.0)) IM = I
  END DO
!    
! Numerov algorithm from left to right and vice versa
!
  NL = IM+1
  NR = N-IM+2
  CALL NMRV2 (NL,H,QL,S,UL)
  CALL NMRV2 (NR,H,QR,S,UR)
!
! Rescale the left solution
!
  FACT = UR(NR-1)/UL(IM)
  DO I = 1, NL
    UL(I) = FACT*UL(I)
  END DO
!
  F0 = UR(NR)+UL(NL)-UR(NR-2)-UL(NL-2)
  F0 = F0/(2.0*H*UR(NR-1))
!
! Bisection method for the root
!
  ISTEP = 0
  DO WHILE ((ABS(DE).GT.DL).AND.(ISTEP.LT.IMAX))
    E1 = E
    E  = (EA+EB)/2.0
    DO I = 1, N
      QL(I) = QL(I)+C*(E-E1)
      QR(I) = QR(I)+C*(E-E1)
    END DO
!
! Find the matching point at the right turning point
!
    DO I = 1, N-1
      IF(((QL(I)*QL(I+1)).LE.0).AND.(QL(I).GT.0)) IM = I
    END DO
!    
! Numerov algorithm from left to right and vice versa
!
    NL = IM+1
    NR = N-IM+2
    CALL NMRV2 (NL,H,QL,S,UL)
    CALL NMRV2 (NR,H,QR,S,UR)
!
! Rescale the left solution
!
    FACT = UR(NR-1)/UL(IM)
    DO I = 1, NL
          UL(I) = FACT*UL(I)
    END DO
!
    F1 = UR(NR)+UL(NL)-UR(NR-2)-UL(NL-2)
    F1 = F1/(2.0*H*UR(NR-1))
!
    IF ((F0*F1).LT.0) THEN
      EB = E 
      DE = EB-EA
    ELSE
      EA = E
      DE = EB-EA
      F0 = F1
    END IF
    ISTEP = ISTEP+1
  END DO
!
  SUM = 0.0
  DO I = 1, N
    IF(I.GT.IM) UL(I) = UR(N-I)
    SUM = SUM+UL(I)*UL(I)
  END DO
!
  WRITE(6,"(2I4)") ISTEP,IMAX
  WRITE(6,"(4F20.8)") E,DE,F0,F1
!
  SUM=SQRT(H*SUM)
  DO I = 1, N, M
    XL = XL0+(I-1)*H
    UL(I) = UL(I)/SUM
    WRITE(15,"(4F20.8)") XL,UL(I)
    WRITE(16,"(4F20.8)") XL,VX(XL)
  END DO
END PROGRAM SCHR
!
FUNCTION VX (X) RESULT (V)
  REAL :: A,B,X,V
!
  A = 1.0
  B = 4.0
  V = 3.0-A*A*B*(B-1.0)/(COSH(A*X)**2)/2.0
END FUNCTION VX
!
SUBROUTINE NMRV2 (N,H,Q,S,U)
!
! The Numerov algorithm for the equation u"(x)+q(x)u(x)=s(x)
! as given in Eqs. (3.82)-(3.85) in the book.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: I
  REAL,INTENT (IN) :: H
  REAL :: G,C0,C1,C2,D,UTMP
  REAL, INTENT (IN), DIMENSION (N) :: Q,S
  REAL, INTENT (INOUT), DIMENSION (N) :: U
!
  G = H*H/12.0
!
  DO I = 2, N-1
    C0 = 1.0+G*Q(I-1)
    C1 = 2.0-10.0*G*Q(I)
    C2 = 1.0+G*Q(I+1)
    D  = G*(S(I+1)+S(I-1)+10.0*S(I))
    UTMP   = C1*U(I)-C0*U(I-1)+D
    U(I+1) = UTMP/C2
  END DO
END SUBROUTINE NMRV2
