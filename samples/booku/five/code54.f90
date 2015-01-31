!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 5.4   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
SUBROUTINE LGND (LMAX,X,P)
!
! Subroutine to generate Legendre polynomials P_L(X)
! for L = 0,1,...,LMAX with given X.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: LMAX
  INTEGER :: L
  REAL, INTENT (IN) :: X
  REAL, INTENT (OUT), DIMENSION (0:LMAX) :: P
!
  P(0) = 1.0
  P(1) = X
  DO L = 1, LMAX-1
    P(L+1) = ((2.0*L+1)*X*P(L)-L*P(L-1))/(L+1)
  END DO
END SUBROUTINE LGND
