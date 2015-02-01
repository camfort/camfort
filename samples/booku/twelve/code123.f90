!!!!!!!!!!!!!!!!!!!!!!!!!!    Program 12.3    !!!!!!!!!!!!!!!!!!!!!!!!!!
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
MODULE ORDER_AN_ARRAY
  PRIVATE EXCHANGE
!
  CONTAINS
!
  SUBROUTINE REARRANGE (A)
    IMPLICIT NONE
    REAL, INTENT(INOUT) :: A(:)
    LOGICAL, ALLOCATABLE, unit(1) :: MASK(:)
    INTEGER :: I
    INTEGER, unit(1) :: N
    INTEGER, DIMENSION(1) :: K
    N = SIZE (A)
    ALLOCATE (MASK(N))
    MASK = .TRUE.
    DO I = 0, N-1
      MASK(N-I) = .FALSE.
       K  = MAXLOC(A,MASK)
      CALL EXCHANGE(A(K(1)),A(N-I))
    END DO
  END SUBROUTINE REARRANGE
! 
  SUBROUTINE EXCHANGE (X,Y)
    IMPLICIT NONE
    REAL, INTENT(INOUT):: X,Y
    REAL TX
    TX = X; X = Y; Y = TX
  END SUBROUTINE EXCHANGE
!
END MODULE ORDER_AN_ARRAY
!
PROGRAM RANDOM_ARRAY_ORDERED
  USE ORDER_AN_ARRAY
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N = 100
  REAL, DIMENSION(N) :: A
  INTEGER :: I
!
  CALL RANDOM_NUMBER (A) 
  CALL REARRANGE (A)
  WRITE(6, "(F10.8)") (A(I),I=1,N)
END PROGRAM RANDOM_ARRAY_ORDERED
