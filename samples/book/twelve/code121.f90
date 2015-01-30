!!!!!!!!!!!!!!!!!!!!!!!!!!    Program 12.1    !!!!!!!!!!!!!!!!!!!!!!!!!!
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
SUBROUTINE RPHI_TO_XY (R,PHI,X,Y)
  IMPLICIT NONE
  REAL, INTENT (IN) :: R,PHI
  REAL, INTENT (OUT) :: X,Y
  X = R*COS(PHI); Y = R*SIN(PHI) ! Assign x and y
END SUBROUTINE RPHI_TO_XY
