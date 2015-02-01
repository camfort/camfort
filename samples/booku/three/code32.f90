!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 3.12  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
MODULE CB
  REAL, unit (q) :: Q
  REAL, unit (1) :: B
  REAL, unit (w) :: W
END MODULE CB
!
PROGRAM PENDULUM
!
! Main Program for a driven pendulum under damping solved with
! the fourth-order Runge-Kutta algorithm.  Parameters: Q, B,
! and W (omega_0).  Copyright (c) Tao Pang 1997.
!
  USE CB
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=1000,M=1
  INTEGER, PARAMETER, unit(l) :: L=100
  INTEGER :: I
  REAL :: PI,H,T,Y1,Y2,G1,G1F,G2,G2F
  REAL :: DK11,DK21,DK12,DK22,DK13,DK23,DK14,DK24
  REAL, DIMENSION (2,N) :: Y
!
  PI = 4.0*ATAN(1.0)
  H  = 3.0*PI/L
  Q  = 0.5
  B  = 0.9
  W  = 2.0/3.0
  Y(1,1) = 0.0
  Y(2,1) = 2.0
!
! Using the Runge-Kutta algorithm to integrate the equation
!
  DO I = 1, N-1
    T  = H*I
    Y1 = Y(1,I)
    Y2 = Y(2,I)
    DK11 = H*G1F(Y1,Y2,T)
    DK21 = H*G2F(Y1,Y2,T)
    DK12 = H*G1F((Y1+DK11/2.0),(Y2+DK21/2.0),(T+H/2.0))
    DK22 = H*G2F((Y1+DK11/2.0),(Y2+DK21/2.0),(T+H/2.0))
    DK13 = H*G1F((Y1+DK12/2.0),(Y2+DK22/2.0),(T+H/2.0))
    DK23 = H*G2F((Y1+DK12/2.0),(Y2+DK22/2.0),(T+H/2.0))
    DK14 = H*G1F((Y1+DK13),(Y2+DK23),(T+H))
    DK24 = H*G2F((Y1+DK13),(Y2+DK23),(T+H))
    Y(1,I+1) = Y(1,I)+(DK11+2.0*(DK12+DK13)+DK14)/6.0
    Y(2,I+1) = Y(2,I)+(DK21+2.0*(DK22+DK23)+DK24)/6.0
!
! Bring theta back to the region [-pi,pi]
!
    Y(1,I+1) = Y(1,I+1)-2.0*PI*NINT(Y(1,I+1)/(2.0*PI))
  END DO
  WRITE (6,"(2F16.8)") (Y(1,I),Y(2,I),I=1,N,M)
END PROGRAM PENDULUM
!
FUNCTION G1F (Y1,Y2,T) RESULT (G1)
  USE CB
  IMPLICIT NONE
  REAL :: Y1,Y2,T,G1
!
  G1 = Y2
END FUNCTION G1F
!
FUNCTION G2F (Y1,Y2,T) RESULT (G2)
  USE CB
  IMPLICIT NONE
  REAL :: Y1,Y2,T,G2
!
  G2 = -Q*Y2-SIN(Y1)+B*COS(W*T)
END FUNCTION G2F
