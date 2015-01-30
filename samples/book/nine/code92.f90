!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 9.2   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
MODULE CB
  INTEGER :: ICPT
  REAL :: X,B,W
END MODULE CB
!
PROGRAM MCMA
!
! Integral evaluated with the Metropolis Monte Carlo scheme.  The
! distribution function is W(x)= (exp(x*x)-1)/Z and the function
! sampled is is g(x) = Z*x*x/(exp(x*x)-1).  Copyright (c) Tao Pang 1997.
!
  USE CSEED
  USE CB
  IMPLICIT NONE
  INTEGER, PARAMETER :: ITHM=10000,ISIZ=10000,L=15
  INTEGER :: time,STIME,I
  INTEGER, DIMENSION (9) :: T
  REAL :: SUM1,SUM2,S,DS,Z,ACPT,G,GX,R,RANF,WX,WFNT
!
  B = 0.4
  Z = 0.46265167
!
! Initial seed from the system time and and forced to be odd
!
  STIME = time(%REF(0))
  CALL gmtime(STIME,T)
  ISEED = T(6)+70*(T(5)+12*(T(4)+31*(T(3)+23*(T(2)+59*T(1)))))
  IF (MOD(ISEED,2).EQ.0) ISEED = ISEED-1
!
! Runs to remove the initial configuration dependence
!
  X = RANF()
  W = WFNT(X)
  DO I = 1, ITHM
    CALL MTSP
  END DO
!
! Collect data at every L steps
!
  SUM1 = 0.0
  SUM2 = 0.0
  ICPT = 0
  DO I = 1, ISIZ*L
    CALL MTSP
    IF (MOD(I,L).EQ.0) THEN
      SUM1 = SUM1+GX(X)
      SUM2 = SUM2+GX(X)*GX(X)
    ELSE
    END IF
  END DO
!
  S  = Z*SUM1/ISIZ
  DS = Z*SQRT(ABS(SUM2/ISIZ-(SUM1/ISIZ)**2)/ISIZ)
  ACPT = 100.0*ICPT/(ISIZ*L)
!
  WRITE(6,"(3F16.8)") S,DS,ACPT
END PROGRAM MCMA
!
SUBROUTINE MTSP
!
! Subroutine for one Metropolis step.  Copyright (c) Tao Pang 1997.
!
  USE CB
  IMPLICIT NONE
  REAL :: XSAV,RNDX,RNDW,WTRY,R,RANF,WX,WFNT
!
  XSAV = X
!
  RNDX = RANF()
  X = X+2.0*B*(RNDX-0.5)
  IF ((X.LT.0).OR.(X.GT.1)) THEN
    X = XSAV
  ELSE
    WTRY = WFNT(X)
    RNDW = RANF()
    IF (WTRY.GT.(W*RNDW)) THEN
      W    = WTRY
      ICPT = ICPT+1
    ELSE
      X    = XSAV
    END IF
  END IF
END SUBROUTINE MTSP
!
FUNCTION GX(X) RESULT (G)
  IMPLICIT NONE
  REAL :: X,G
  G = X*X/(EXP(X*X)-1.0)
END FUNCTION GX
!
FUNCTION WFNT(X) RESULT (WX)
  IMPLICIT NONE
  REAL :: X,WX
        WX = EXP(X*X)-1.0
END FUNCTION WFNT
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
