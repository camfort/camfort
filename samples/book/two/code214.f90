!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 2.14   !!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
!MODULE CSEED
! INTEGER :: ISEED
!END MODULE CSEED
!
SUBROUTINE PERCOLATION (L,N,M,P)
!
! Subroutine to create an N*M percolation network.
! Copyright (c) Tao Pang 1997.
!
  INTEGER, INTENT (IN) :: N,M
  REAL, INTENT (IN) :: P
  REAL:: R,RANF
  INTEGER I,J
  LOGICAL, INTENT (OUT), DIMENSION (N,M) :: L
!
  DO I = 1, N
    DO J = 1, M
      R = RANF()
      IF(R.LT.P) THEN
        L(I,J) = .TRUE.
      ELSE
        L(I,J) = .FALSE.
      END IF
    END DO
  END DO
END SUBROUTINE PERCOLATION
!
FUNCTION RANF() RESULT (R)
!
! Uniform random number generator x(n+1) = a*x(n) mod c with
! a=7**5 and c = 2**(31)-1.  Copyright (c) Tao Pang 1997.
!
!  USE CSEED
  IMPLICIT NONE
 INTEGER :: ISEED
  INTEGER :: IH,IL,IT,IA,IC,IQ,IR
  DATA IA/16807/,IC/2147483647/,IQ/127773/,IR/2836/
  REAL :: R
!
  IH = ISEED/IQ
  IL = MOD(ISEED,IQ)
  IT = IA*IL-IR*IH
  IF(IT.GT.0) THEN
    ISEED = IT
  ELSE
    ISEED = IC+IT
  END IF
  R = ISEED/FLOAT(IC)
END FUNCTION RANF
