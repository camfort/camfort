!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 3.1   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM ONE_D_MOTION2
!
! Simplest predictor-corector algorithm applied to a particle in one
! dimension under an elastic force.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(n) :: N=101
  INTEGER, PARAMETER, unit(i) :: IN=5
  INTEGER :: I
  REAL, unit(pi) :: PI
  REAL :: DT
  REAL, DIMENSION (N) :: T,X
  REAL, DIMENSION (N), unit(v) :: V
!
  PI  = 4.0*ATAN(1.0)
  DT  =2.0*PI/100
  X(1)=0.0
  T(1)=0.0
  V(1)=1.0
!
  DO I = 1, N-1
    T(I+1) = I*DT
!
! Predictor for position and velocity
!
     X(I+1) = X(I)+V(I)*DT
     V(I+1) = V(I)-X(I)*DT
!
! Corrector for position and velocity
!
     X(I+1) = X(I)+(V(I)+V(I+1))*DT/2.0
     V(I+1) = V(I)-(X(I)+X(I+1))*DT/2.0
  END DO
  WRITE(6,"(3F16.8)") (T(I),X(I),V(I),I=1,N,IN)
END PROGRAM ONE_D_MOTION2
