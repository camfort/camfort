!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 3.A   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
SUBROUTINE NMRV (N,H,Q,S,U)
!
! The Numerov algorithm for the equation u"(x)+q(x)u(x)=s(x)
! as given in Eqs. (3.77)-(3.80) in the book.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER, unit(i) :: I
  REAL, INTENT (IN) :: H
  REAL :: G,C0,C1,C2,D,UTMP
  REAL, INTENT (IN), DIMENSION (N) :: Q,S
  REAL, INTENT (OUT), DIMENSION (N) :: U
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
