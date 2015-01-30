!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 5.1   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM DFT_EXAMPLE
!
! Example of the discrete Fourier transform with function f(x) =
! x(1-x) in [0,1]. The inverse transform is also performed for
! comparison.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=128,M=8
  INTEGER :: I
  REAL :: F0,H,X
  REAL, DIMENSION (N) :: FR,FI,GR,GI
!
  F0 = 1.0/SQRT(FLOAT(N))
  H  = 1.0/(N-1)
!
  DO I = 1, N
    X = H*(I-1)
    FR(I) = X*(1.0-X)
    FI(I) = 0.0
  END DO
!
  CALL DFT (FR,FI,GR,GI,N)
  DO I = 1, N
    GR(I) = F0*GR(I)
    GI(I) = F0*GI(I)
  END DO
!
! Perform inverse Fourier transform
!
  DO I = 1, N
    GI(I) = -GI(I)
  END DO
  CALL DFT (GR,GI,FR,FI,N)
  DO I = 1, N
    FR(I) =  F0*FR(I)
    FI(I) = -F0*FI(I)
  END DO
  WRITE (6,"(2F16.8)") (H*(I-1),FR(I),I=1,N,M)
  WRITE (6,"(2F16.8)") H*(N-1),FR(N)
END PROGRAM DFT_EXAMPLE
!
SUBROUTINE DFT (FR,FI,GR,GI,N)
!
! Subroutine to perform the discrete Fourier transform with
! FR and FI as the real and imaginary parts of the input and
! GR and GI as the corresponding  output.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: I,J
  REAL :: PI,X,Q
  REAL, INTENT (IN), DIMENSION (N) :: FR,FI
  REAL, INTENT (OUT), DIMENSION (N) :: GR,GI
!
  PI = 4.0*ATAN(1.0)
  X  = 2*PI/N
!
  DO I = 1, N
    GR(I) = 0.0
    GI(I) = 0.0
    DO J = 1, N
      Q = X*(J-1)*(I-1)
      GR(I) = GR(I)+FR(J)*COS(Q)+FI(J)*SIN(Q)
      GI(I) = GI(I)+FI(J)*COS(Q)-FR(J)*SIN(Q)
    END DO
  END DO
END SUBROUTINE DFT
