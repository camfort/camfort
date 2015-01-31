!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 8.1   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM GALERKIN
!
! This program solves the one-dimensional Poisson equation with the
! Galerkin method as described in the text.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=99
  INTEGER :: I
  REAL :: PI,XL,H,D,E,B0,B1,XIM,XI,XIP
  REAL, DIMENSION (N) :: B,A,Y,W,U
!
  PI  =  4.0*ATAN(1.0)
  XL  =  1.0
  H   =  XL/(N+1)
  D   =  2.0/H
  E   = -1.0/H
  B0  =  PI/H
  B1  =  1.0/H
!
! Find the elements in L and U
!
  W(1) =  D
  U(1) =  E/D
  DO I = 2, N
    W(I) = D-E*U(I-1)
    U(I) = E/W(I)
  END DO
!
! Assign the array B
!
  DO I = 1, N
    XIM  = H*(I-1)
    XI   = H*I
    XIP  = H*(I+1)
    B(I) = B0*COS(PI*XI)*(XIM+XIP-2.0*XI) &
          +B1*(2.0*SIN(PI*XI)-SIN(PI*XIM)-SIN(PI*XIP))
  END DO
!
! Find the solution
!
  Y(1) = B(1)/W(1)
  DO I = 2, N
    Y(I) = (B(I)-E*Y(I-1))/W(I)
  END DO
!
  A(N) = Y(N)
  DO I = N-1,1,-1
    A(I) = Y(I)-U(I)*A(I+1)
  END DO
!
  WRITE (6,"(2F16.8)") (I*H,A(I), I=1,N)
END PROGRAM GALERKIN
