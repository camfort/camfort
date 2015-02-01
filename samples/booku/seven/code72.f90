!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 7.2   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
MODULE CSEED
  INTEGER, unit(i) :: ISEED
END MODULE CSEED
!
SUBROUTINE MXWL (N,M,T,V)
!
! This subroutine assigns velocities according to the Maxwell distribution.
! N is the total number of velocity components and M is the total number of
! degrees of freedom.  T is the system temperature in the reduced units.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N,M
  INTEGER, unit(i) :: I
  REAL, INTENT (IN) :: T
  REAL :: V1,V2,EK,VS
  REAL, INTENT (OUT), DIMENSION (N) :: V
!
! Assign a Gaussian distribution to each velocity component
!
  DO I = 1, N-1, 2
    CALL GRNF (V1,V2)
    V(I)   = V1
    V(I+1) = V2
  END DO
!
! Scale the velocity to satisfy the partition theorem
!
  EK = 0.0
  DO I = 1, N
    EK = EK+V(I)*V(I)
  END DO
  VS = SQRT(EK/(M*T))
  DO I = 1, N
    V(I) = V(I)/VS
  END DO
END SUBROUTINE MXWL
!
SUBROUTINE GRNF (X,Y)
!
! Two Gaussian random numbers generated from two uniform random
! numbers. Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  REAL, INTENT (OUT) :: X,Y
  REAL :: R ! RANF
  REAL, unit(p) ::  PI
  REAL, unit(ra) :: R1
  REAL, unit(rb) :: R2
!
  PI = 4.0*ATAN(1.0)
  R1 = -ALOG(1.0-RANF())
  R2 = 2.0*PI*RANF()
  R1 = SQRT(2.0*R1)
  X  = R1*COS(R2)
  Y  = R1*SIN(R2)
END SUBROUTINE GRNF
!
FUNCTION RANF() !RESULT (R)
!
! Uniform random number generator x(n+1) = a*x(n) mod c with
! a=7**5 and c = 2**(31)-1.  Copyright (c) Tao Pang 1997.
!
  USE CSEED
  IMPLICIT NONE
  INTEGER :: IH,IL,IT,IC,IQ
  INTEGER, unit(r) :: IR
  INTEGER, unit(a) :: IA
  DATA IA/16807/,IC/2147483647/,IQ/127773/,IR/2836/
  !REAL :: R
!
  IH = ISEED/IQ
  IL = MOD(ISEED,IQ)
  IT = IA*IL-IR*IH
  IF(IT.GT.0) THEN
    ISEED = IT
  ELSE
    ISEED = IC+IT
  END IF
  RANF = ISEED/FLOAT(IC)
END FUNCTION RANF
