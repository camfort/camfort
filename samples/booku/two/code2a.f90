!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.A   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM INTERPOLATION2
!
! Main program for the Lagrange interpolation with the
! upward and downward correction method.
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
  CALL UPDOWN (N,XI,FI,X,F,DF)
  WRITE (6,"(3F16.8)") X,F,DF
END PROGRAM INTERPOLATION2
!
SUBROUTINE UPDOWN (N,XI,FI,X,F,DF)
!
! Subroutine performing the Lagrange interpolation with the
! upward and downward correction method.  F: interpolated
! value.  DF: error estimated.  Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(nmax) :: NMAX=21
  INTEGER, INTENT (IN) :: N
  INTEGER :: I,J,I0,J0,K
  INTEGER, unit(i) :: IT
  REAL, INTENT (IN) :: X
  REAL, INTENT (OUT) :: F,DF
  REAL, unit(dx) :: DX
  REAL, unit (dt) :: DT
  REAL :: DXT
  REAL, INTENT (IN), DIMENSION (N) :: XI,FI
  REAL, DIMENSION (NMAX,NMAX) :: DP
  REAL, DIMENSION (NMAX,NMAX), unit(dt dx) :: DM
!
  IF (N.GT.NMAX) STOP 'Dimension of the data set is too large.'
    DX = ABS(XI(N)-XI(1))
    DO  I = 1, N
      DP(I,I) = FI(I)
      DM(I,I) = FI(I)
      DXT = ABS(X-XI(I))
      IF (DXT.LT.DX) THEN
        I0 = I
        DX = DXT
      END IF
    END DO
    J0 = I0
!
! Evaluate correction matrices
!
  DO I = 1, N-1
    DO J = 1, N-I
      K = J+I
      DT =(DP(J,K-1)-DM(J+1,K))/(XI(K)-XI(J))
      DP(J,K) = DT*(XI(K)-X)
      DM(J,K) = DT*(XI(J)-X)
    END DO
  END DO
!
! Update the approximation
!
  F = FI(I0)
  IT = 0
  IF(X.LT.XI(I0)) IT = 1
  DO I = 1, N-1
    IF ((IT.EQ.1).OR.(J0.EQ.N)) THEN
      I0 = I0-1
      DF = DP(I0,J0)
      F  = F+DF
      IT = 0
      IF (J0.EQ.N) IT = 1
    ELSE IF ((IT.EQ.0).OR.(I0.EQ.1)) THEN
      J0 = J0+1
      DF = DM(I0,J0)
      F  = F+DF
      IT = 1
      IF (I0.EQ.1) IT = 0
    END IF
  END DO
  DF = ABS(DF)
END SUBROUTINE UPDOWN
