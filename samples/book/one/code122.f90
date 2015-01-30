!!!!!!!!!!!!!!!!!!!!!!!!!!    Program 12.2    !!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM ARRAY_EXAMPLE
  IMPLICIT NONE
  REAL :: TWO_PI
  REAL, ALLOCATABLE :: A(:,:), B(:,:), C(:,:), D(:)
  INTEGER :: N,M,L,I
!
  TWO_PI = 8.0*ATAN(1.0)
  READ "(3I4)", N, M, L
  ALLOCATE (A(N,M)); ALLOCATE (B(L,N))
  ALLOCATE (C(L,M)); ALLOCATE (D(M))
  CALL RANDOM_NUMBER (A); CALL RANDOM_NUMBER (B)
  A = SIN(TWO_PI*A); B = COS(TWO_PI*B)
  C = MATMUL(B,A)
  DO      I = 1, M
    D(I) = DOT_PRODUCT(A(:,I),B(I,:))
  END DO
  PRINT "(8F10.6)", D
END PROGRAM ARRAY_EXAMPLE
