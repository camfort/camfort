!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 6.2   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
SUBROUTINE RLXN (FN,DN,S,N,P,H)
!
! Subroutine performing one iteration of Relaxation for the one-dimensional
! stationary diffusion equation. DN is the diffusion coefficient shifted
! half step towards x=0.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER, unit(i) :: I
  REAL, INTENT (IN) :: H,P
  REAL :: H2,Q
  REAL, INTENT (IN), DIMENSION (N) :: DN,S
  REAL, INTENT (INOUT), DIMENSION (N) :: FN
!
  H2 = H*H
  Q  = 1.0-P
  DO I = 2, N-1
    FN(I) = Q*FN(I)+P*(DN(I+1)*FN(I+1)+DN(I)*FN(I-1)+H2*S(I))/(DN(I+1)+DN(I))
  END DO
END SUBROUTINE RLXN
