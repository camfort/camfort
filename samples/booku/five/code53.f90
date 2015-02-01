!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 5.3   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
SUBROUTINE FFT2D (FR,FI,N1,N2,M1,M2)
!
! Subroutine for the two-dimensional fast Fourier transform
! with N=N1*N2 and N1=2**M1 and N2=2**M2.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N1,N2,M1,M2
  INTEGER :: I,J
  REAL, INTENT (INOUT), DIMENSION  (N1,N2) :: FR,FI
!
! Transformation on the second index
!
  DO I = 1, N1
    CALL FFT (FR(I,1),FI(I,1),N2,M2)
  END DO
!
! Transformation on the first index
!
  DO J = 1, N2
    CALL FFT (FR(1,J),FI(1,J),N1,M1)
  END DO
END SUBROUTINE FFT2D
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
  REAL, unit(1) :: PI
  REAL, unit(a) :: A1
  REAL :: A2,Q,U,V
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
