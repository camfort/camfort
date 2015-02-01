!!!!!!!!!!!!!!!!!!!!!!!!!!    Program 12.4    !!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                      !
! Please Note:                                                         !
!                                                                      !
! (1) This computer program is part of the book, "An Introduction to   !
!     Computational Physics," written by Tao Pang and published and    !
!     copyrighted by Cambridge University Press in 1997.               !
!                                                                      !
! (2) No warranties, express or implied, are made for this program.    !
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
PROGRAM LAPLACE_2D
!
! Example program for the 2D Laplace equation as defined
! in the text.
!
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(nx) :: NX=101
  INTEGER, PARAMETER, unit(ny) :: NY=101
  INTEGER I, J, IMAX
  REAL, DIMENSION (NX,NY) :: BOUND,U,UTMP, DU
  REAL, DIMENSION (NX,NY), unit(f) ::FIELD
  REAL :: TL,Q,P,A,B
  LOGICAL, DIMENSION (NX,NY), unit(m) :: MASK 
!
  TL = 1.0E-08
  A  = 1.0/(NY-1)
  B  = 1.0/((NY-1)**2)
  P  = 0.5
  Q  = 1.0 - P
!
! Align the data accordingly
!
!HPF$ DISTRIBUTE U(BLOCK,BLOCK)
!
!HPF$ ALIGN BOUND(:,:) WITH U(:,:)
!HPF$ ALIGN FIELD(:,:) WITH U(:,:)
!HPF$ ALIGN UTMP(:,:)  WITH U(:,:)
!HPF$ ALIGN MASK(:,:)  WITH U(:,:)
!
  MASK  = .FALSE.
  MASK(2:NX-1,2:NY-1) = .TRUE.
!
  BOUND = 0.0
  FIELD = 0.0
  WHERE (MASK)
    FIELD = 0.25
  ELSEWHERE
    BOUND = 0.25
  END WHERE
!
! Assign the initial value for iteration
!
  FORALL (I=1:NX,J=1:NY)  U(I,J) = A*(J-1)*(NX-I)/(NX-1) &
                                  +B*(J-1)**2*(I-1)/(NX-1)
!
! The main iteration loop
!
  DO  I = 1, IMAX
    UTMP = FIELD*( CSHIFT(U,SHIFT= 1,DIM=1) &
                 + CSHIFT(U,SHIFT= 1,DIM=2) &
                 + CSHIFT(U,SHIFT=-1,DIM=1) &
                 + CSHIFT(U,SHIFT=-1,DIM=2) ) &
         + BOUND
    DU   = U - UTMP
    U    = Q*U + P*UTMP
    DU   = MAXVAL(ABS(U - UTMP))
    IF (DU.LE.TL) EXIT
  END DO
  IF (I.NE.IMAX) THEN
    PRINT*, 'The number of iteration is ', I, &
' and the largest error is ', DU
    PRINT*, 'The solution is ', U
  ELSE
    PRINT*, 'The required convergence is not reached yet'
  END IF
END PROGRAM LAPLACE_2D
