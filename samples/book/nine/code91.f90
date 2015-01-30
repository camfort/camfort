!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 9.1   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
  INTEGER :: ISEED
END MODULE CSEED
!
PROGRAM MCDS
!
! Integration with the direct sampling Monte Carlo scheme.  The integrand
! is f(x) = x*x.  Copyright (c) Tao Pang 1997.
!
  USE CSEED
  IMPLICIT NONE
  INTEGER, PARAMETER :: M=1000000
  INTEGER :: time,STIME,I
  INTEGER, DIMENSION (9) :: T
  REAL :: SUM1,SUM2,S,DS,X,F,FX,R,RANF
!
! Initial seed from the system time and forced to be odd
!
  STIME = time(%REF(0))
  CALL gmtime(STIME,T)
  ISEED = T(6)+70*(T(5)+12*(T(4)+31*(T(3)+23*(T(2)+59*T(1)))))
  IF (MOD(ISEED,2).EQ.0) ISEED = ISEED-1
!
  SUM1 = 0.0
  SUM2 = 0.0
  DO I = 1, M
    X = RANF()
    SUM1 = SUM1+FX(X)
    SUM2 = SUM2+FX(X)**2
  END DO
  S  = SUM1/M
  DS = SQRT(ABS(SUM2/M-(SUM1/M)**2)/M)
  WRITE(6,"(2F16.8)") S,DS
END PROGRAM MCDS
!
FUNCTION FX(X) RESULT (F)
  IMPLICIT NONE
  REAL :: X,F
!
  F = X*X
END FUNCTION FX
!
FUNCTION RANF() RESULT (R)
!
! Uniform random number generator x(n+1) = a*x(n) mod c with
! a=7**5 and c = 2**(31)-1.  Copyright (c) Tao Pang 1997.
!
  USE CSEED
  IMPLICIT NONE
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
