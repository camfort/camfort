!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 5.A   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
  REAL :: Q,B,W
END MODULE CB
!
PROGRAM PENDULUM
!
! Program for the power spectra of a driven pendulum under damping with
! the fourth order Runge-Kutta algorithm. Given parameters: Q, B, and W
! (omega_0).  Copyright (c) Tao Pang 1997.
!
  USE CB
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=65536,L=128,M=16,MD=16
  INTEGER :: I,J
  REAL :: PI,F1,H,OD,T,Y1,Y2,G1,GX1,G2,GX2
  REAL :: DK11,DK21,DK12,DK22,DK13,DK23,DK14,DK24
  REAL, DIMENSION (N) :: AR,AI,WR,WI,O
  REAL, DIMENSION (2,N) :: Y

!
  PI = 4.0*ATAN(1.0)
  F1 = 1.0/SQRT(FLOAT(N))
  W  = 2.0/3.0
  H  = 2.0*PI/(L*W)
  OD = 2.0*PI/(N*H*W*W)
  Q  = 0.5
  B  = 1.15
  Y(1,1) = 0.0
  Y(2,1) = 2.0
!
! Runge-Kutta algorithm to integrate the equation
!
  DO I = 1, N-1
    T  = H*I
    Y1 = Y(1,I)
    Y2 = Y(2,I)
    DK11 = H*GX1(Y1,Y2,T)
    DK21 = H*GX2(Y1,Y2,T)
    DK12 = H*GX1((Y1+DK11/2.0),(Y2+DK21/2.0),(T+H/2.0))
    DK22 = H*GX2((Y1+DK11/2.0),(Y2+DK21/2.0),(T+H/2.0))
    DK13 = H*GX1((Y1+DK12/2.0),(Y2+DK22/2.0),(T+H/2.0))
    DK23 = H*GX2((Y1+DK12/2.0),(Y2+DK22/2.0),(T+H/2.0))
    DK14 = H*GX1((Y1+DK13),(Y2+DK23),(T+H))
    DK24 = H*GX2((Y1+DK13),(Y2+DK23),(T+H))
    Y(1,I+1) = Y(1,I)+(DK11+2.0*(DK12+DK13)+DK14)/6.0
    Y(2,I+1) = Y(2,I)+(DK21+2.0*(DK22+DK23)+DK24)/6.0
!
! Bring theta back to region [-pi,pi]
!
     IF (ABS(Y(1,I+1)).GT.PI) THEN
       Y(1,I+1) = Y(1,I+1) - 2.*PI*ABS(Y(1,I+1))/Y(1,I+1)
     END IF
  END DO
!
  DO I = 1, N
    AR(I) = Y(1,I)
    WR(I) = Y(2,I)
    AI(I) = 0.0
    WI(I) = 0.0
  END DO
  CALL FFT (AR,AI,N,M)
  CALL FFT (WR,WI,N,M)
!
  DO I = 1, N
    O(I)  =  (I-1)*OD
    AR(I) =  (F1*AR(I))**2+(F1*AI(I))**2
    WR(I) =  (F1*WR(I))**2+(F1*WI(I))**2
    AR(I) = ALOG10(AR(I))
    WR(I) = ALOG10(WR(I))
  END DO
  WRITE(6,"(3F16.10)") (O(I),AR(I),WR(I),I=1,(L*MD),4)
END PROGRAM PENDULUM
!
  SUBROUTINE FFT (AR,AI,N,M)
!
! An example of the fast Fourier transform subroutine with N = 2**M.
! AR and AI are the real and imaginary part of data in the input and
! corresponding Fourier coefficients in the output.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N,M
  INTEGER :: N1,N2,I,J,K,L,L1,L2
  REAL :: PI,A1,A2,Q,U,V
  REAL, INTENT (INOUT), DIMENSION (N) :: AR,AI
!
  PI = 4.0*ATAN(1.0)
  N2 = N/2
!
  N1 = 2**M
  IF(N1.NE.N) STOP 'Indices do not match'
!
! Rearrange the data to the bit reversed order
!
  L = 1
  DO K = 1, N-1
    IF (K.LT.L) THEN
      A1    = AR(L)
      A2    = AI(L)
      AR(L) = AR(K)
      AR(K) = A1
      AI(L) = AI(K)
      AI(K) = A2
    END IF
    J   = N2
    DO WHILE (J.LT.L)
      L = L-J
      J = J/2
    END DO
    L = L+J
  END DO
!
! Perform additions at all levels with reordered data
!
  L2 = 1
  DO L = 1, M
    Q  =  0.0
    L1 =  L2
    L2 =  2*L1
    DO K = 1, L1
      U   =  COS(Q)
      V   = -SIN(Q)
      Q   =  Q + PI/L1
      DO J = K, N, L2
        I     =  J + L1
        A1    =  AR(I)*U-AI(I)*V
        A2    =  AR(I)*V+AI(I)*U
        AR(I) =  AR(J)-A1
        AR(J) =  AR(J)+A1
        AI(I) =  AI(J)-A2
        AI(J) =  AI(J)+A2
      END DO
    END DO
  END DO
END SUBROUTINE FFT
!
FUNCTION GX1 (Y1,Y2,T) RESULT (G1)
!
  G1 = Y2
END FUNCTION GX1
!
FUNCTION GX2 (Y1,Y2,T) RESULT (G2)
  USE CB
!
  G2 = -Q*Y2-SIN(Y1)+B*COS(W*T)
END FUNCTION GX2
