!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 4.5   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
SUBROUTINE TDPL(A,B,N,X,P)
!
! Subroutine to generate determinant polynomial P_N(X).
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER, unit(I) :: I
  REAL, INTENT (IN) :: X
  REAL :: P0
  REAL, INTENT (IN), DIMENSION (N) :: A,B
  REAL, INTENT (OUT), DIMENSION (N) :: P
!
  P0  = 1.0
  IF (N.LT.1) STOP 'The dimension is less than 1.'
  P(1) = A(1)-X
  IF (N.GE.2) P(2) = (A(2)-X)*P(1)-B(1)*B(1)*P0
  IF (N.GE.3) THEN
    DO I = 2, N-1
      P(I+1) = (A(I+1)-X)*P(I)-B(I)*B(I)*P(I-1)
    END DO
  END IF
END SUBROUTINE TDPL
