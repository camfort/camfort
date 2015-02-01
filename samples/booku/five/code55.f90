!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 5.5   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
SUBROUTINE BSSL (BJ,BY,N,X)
!
! Subroutine to generate J_n(x) and Y_n(x) with given x and
! up to N=NMAX-NTEL.
!  
  INTEGER, PARAMETER, unit(nm) :: NMAX=30
  INTEGER, PARAMETER, unit(nt) :: NTEL=5
  INTEGER, INTENT (IN) :: N
  INTEGER :: I,J,K
  REAL, INTENT (IN) :: X
  REAL :: SUM1
  REAL, unit(p) :: PI
  REAL, unit(g) :: GAMMA
  REAL, unit(s) :: SUM
  REAL, INTENT (OUT), DIMENSION (0:N) :: BJ,BY
  REAL, DIMENSION (0:NMAX) :: B1
!
  PI = 4.0*ATAN(1.0)
  GAMMA = 0.5772156649
!
  B1(NMAX)   = 0.0
  B1(NMAX-1) = 1.0
!
! Generating J_n(x)
!
  SUM = 0.0
  DO I = NMAX-1, 1, -1 
    B1(I-1) = 2*I*B1(I)/X-B1(I+1)
    IF (MOD(I,2).EQ.0) SUM = SUM+2.0*B1(I)
  END DO
  SUM = SUM+B1(0)
!
  DO I = 0, N
    BJ(I) = B1(I)/SUM
  END DO
!
! Generating Y_n(x) starts here
!
  SUM1 = 0.0
  DO K = 1, NMAX/2
    SUM1 = SUM1+(-1)**K*B1(2*K)/K
  END DO
!
  SUM1 = -4.0*SUM1/(PI*SUM)
  BY(0) = 2.0*(ALOG(X/2.0)+GAMMA)*BJ(0)/PI+SUM1
  BY(1) = (BJ(1)*BY(0)-2.0/(PI*X))/BJ(0)
  DO I  = 1, N-1
    BY(I+1) = 2*I*BY(I)/X-BY(I-1)
  END DO
END SUBROUTINE BSSL
