!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 6.3   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM G_WATER
!
! This program solves the groundwater dynamics problem in the
! rectangular geometry through the relaxation method.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(1) :: NX=101
  INTEGER, PARAMETER, unit(1) :: NY=51
  INTEGER, PARAMETER :: ISKX=10,ISKY=5
  INTEGER, PARAMETER, unit(i) :: ITMX=5
  INTEGER :: I,J,ISTP
  REAL :: PI,A0,B0,H0,CH,SX,SY,HX,HY,P,X,Y
  REAL, DIMENSION (NX,NY) :: PHI,CK,SN
!
  PI = 4.0*ATAN(1.0)
  A0 = 1.0
  B0 = -0.04
  H0 = 200.0
  CH = -20.0
  SX = 1000.0
  SY = 500.0
  HX = SX/(NX-1)
  HY = SY/(NY-1)
  P  = 0.5
!
! Set up boundary conditions and initial guess of the solution
!
  DO I = 1, NX
    X = (I-1)*HX
    DO J = 1, NY
      Y = (J-1)*HY
      SN(I,J) = 0.0
      CK(I,J) = A0+B0*Y
      PHI(I,J) = H0+CH*COS(PI*X/SX)*Y/SY 
    END DO
  END DO
!
  DO ISTP = 1, ITMX
!
! Ensure the boundary conditions by the 4-point formula
!
    DO J = 1, NY
      PHI(1,J)  = (4.0*PHI(2,J)-PHI(3,J))/3.0
      PHI(NX,J) = (4.0*PHI(NX-1,J)-PHI(NX-2,J))/3.0
    END DO
!
    CALL RX2D (PHI,CK,SN,NX,NY,P,HX,HY)
  END DO
!
  DO I = 1, NX, ISKX
    X = (I-1)*HX
    DO J = 1, NY, ISKY
      Y = (J-1)*HY
      WRITE (6,"(3F16.8)") X,Y,PHI(I,J)
    END DO
  END DO
END PROGRAM G_WATER
!
SUBROUTINE RX2D (FN,DN,S,NX,NY,P,HX,HY)
!
! Subroutine performing one iteration of the relaxation for
! the two-dimensional Poisson equation.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: NX,NY
  INTEGER :: I,J
  REAL, INTENT (IN) :: HX,HY,P
  REAL :: HX2,A,B,Q,CIP,CIM,CJP,CJM
  REAL, INTENT (IN), DIMENSION (NX,NY) :: DN,S
  REAL, INTENT (INOUT), DIMENSION (NX,NY) :: FN
!
  HX2 = HX*HX
  A = HX2/(HY*HY)
  B = 1.0/(4.0*(1.0+A))
  Q = 1.0-P
!
  DO I = 2, NX-1
    DO J = 2, NY-1
      CIP = B*(DN(I+1,J)/DN(I,J)+1.0)
      CIM = B*(DN(I-1,J)/DN(I,J)+1.0)
      CJP = A*B*(DN(I,J+1)/DN(I,J)+1.0)
      CJM = A*B*(DN(I,J-1)/DN(I,J)+1.0)
      FN(I,J) = Q*FN(I,J)+P*(CIP*FN(I+1,J)+CIM*FN(I-1,J) &
               +CJP*FN(I,J+1)+CJM*FN(I,J-1)+HX2*S(I,J))
    END DO
  END DO
END SUBROUTINE RX2D
