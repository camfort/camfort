!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 7.1   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM COMET
!
! Program for the time-dependent position of Halley's comet
! with the velocity version of the Verlet algorithm.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=20001,NP=10000,NI =200
  INTEGER :: I
  REAL :: H,KAPPA
  REAL, DIMENSION (N) :: X,Y,VX,VY,GX,GY,T,R
!
! Initialization of the problem
!
  H     = 1.0/NP
  KAPPA = 39.478428
  T(1)  = 0.0
  X(1)  = 1.966843
  Y(1)  = 0.0
  R(1)  = X(1)
  GX(1) = -KAPPA*X(1)/R(1)**3
  GY(1) = 0.0
  VX(1) = 0.0
  VY(1) = 0.815795
!
! Verlet algorithm for the position and velocity
!
  DO I = 1, N-1
    T(I+1)  = I*H
    X(I+1)  = X(I)+H*VX(I)+H*H*GX(I)/2.0
    Y(I+1)  = Y(I)+H*VY(I)+H*H*GY(I)/2.0
    R(I+1)  = SQRT(X(I+1)*X(I+1)+Y(I+1)*Y(I+1))
    GX(I+1) = -KAPPA*X(I+1)/R(I+1)**3
    GY(I+1) = -KAPPA*Y(I+1)/R(I+1)**3
    VX(I+1) = VX(I)+H*(GX(I+1)+GX(I))/2.0
    VY(I+1) = VY(I)+H*(GY(I+1)+GY(I))/2.0
  END DO
  WRITE (6,"(2F16.8)") (T(I),R(I), I=1,N,NI)
END PROGRAM COMET
