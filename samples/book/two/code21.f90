!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.1   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM INTERPOLATION
!
! Main program for the Lagrange interpolation with the Aitken method.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=5
  REAL :: X,F,DF
  REAL, DIMENSION (N) :: XI,FI
  DATA XI/0.0,0.5,1.0,1.5,2.0/, &
       FI/1.0,0.938470,0.765198,0.511828,0.223891/
!
  X = 0.9
  CALL AITKEN (N,XI,FI,X,F,DF)
  WRITE (6,"(3F16.8)") X,F,DF
END PROGRAM INTERPOLATION
!
SUBROUTINE AITKEN (N,XI,FI,X,F,DF)
!
! Subroutine performing the Lagrange interpolation with the
! Aitken method. F: interpolated value. DF: error estimated.
! Copyright (c) Tao Pang 1997.
!
  INTEGER, PARAMETER :: NMAX=21
  INTEGER, INTENT (IN) :: N
  INTEGER :: I,J
  REAL, INTENT (IN) :: X
  REAL, INTENT (OUT) :: F,DF
  REAL :: X1,X2,F1,F2
  REAL, INTENT (IN), DIMENSION (N):: XI,FI
  REAL, DIMENSION (NMAX):: FT
!
  IF (N.GT.NMAX) STOP 'Dimension of the data is too large.'
  DO I = 1, N
    FT(I) = FI(I)
  END DO
!
  DO I = 1, N-1  
    DO J = 1, N-I
      X1 = XI(J)
      X2 = XI(J+I)
      F1 = FT(J)
      F2 = FT(J+1)
      FT(J) = (X-X1)/(X2-X1)*F2+(X-X2)/(X1-X2)*F1
    END DO
  END DO
  F = FT(1) 
  DF = (ABS(F-F1)+ABS(F-F2))/2.0
END SUBROUTINE AITKEN
