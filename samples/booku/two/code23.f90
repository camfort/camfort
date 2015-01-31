!!!!!!!!!!!!!!!!!!!!!!!!!!!   foo 2.3   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM MILLIKAN
!
! Main program for a linear fit of the Millikan experimental
! data on the fundamental charge e_0 from e_n = e_0*n + de.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(n) :: N=15
  INTEGER, PARAMETER :: M=2
  INTEGER :: I
  REAL :: SUM0,SUMT,E0,DE
  REAL, DIMENSION (N) :: X,F
  REAL, DIMENSION (M) :: A
  REAL, DIMENSION (M,N) :: U
  DATA X /4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0, &
          12.0,13.0,14.0,15.0,16.0,17.0,18.0/
  DATA F /6.558,8.206,9.880,11.50,13.14,14.81,16.40,18.04, &
          19.68,21.32,22.96,24.60,26.24,27.88,29.52/
!
  CALL PFIT (N,M,X,F,A,U)
  SUM0 = 0.0
  SUMT = 0.0
  DO I = 1, N
    SUM0 = SUM0+U(1,I)**2
    SUMT = SUMT+X(I)*U(1,I)**2
  END DO
  E0   = A(2)
  DE   = A(1)-A(2)*SUMT/SUM0
  WRITE (6,"(2F16.8)") E0,DE
END PROGRAM MILLIKAN
!
SUBROUTINE PFIT (N,M,X,F,A,U)
!
! Subroutine generating orthonormal polynomials U(M,N) up to
! (M-1)th order and coefficients A(M), for the least squares
! approximation of the function F(N) at X(N). Other variables
! used: G(K) for g_k, H(K) for h_k, S(K) for <u_k|u_k>.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: NMAX=101,MMAX=101
  INTEGER, INTENT (IN) :: N,M
  INTEGER :: I,J
  REAL :: TMP
  REAL, INTENT (IN), DIMENSION (N) :: X,F
  REAL, INTENT (OUT), DIMENSION (M) :: A
  REAL, INTENT (OUT), DIMENSION (M,N) :: U
  REAL, DIMENSION (MMAX) :: G,H,S
!
  IF(N.GT.NMAX) STOP 'Too many points'
  IF(M.GT.MMAX) STOP 'Order too high'
!
! Set up the zeroth order polynomial u_0
!
  DO I = 1, N
    U(1,I) = 1.0
  END DO
  DO I = 1, N
    TMP  = U(1,I)*U(1,I)
    S(1) = S(1)+TMP
    G(1) = G(1)+X(I)*TMP
    A(1) = A(1)+U(1,I)*F(I)
  END DO
  G(1) = G(1)/S(1)
  H(1) = 0.0
  A(1) = A(1)/S(1)
!
! Set up the first order polynomial u_1
!
  DO I = 1, N
    U(2,I) = X(I)*U(1,I)-G(1)*U(1,I)
    S(2)   = S(2)+U(2,I)**2
    G(2)   = G(2)+X(I)*U(2,I)**2
    H(2)   = H(2) + X(I)*U(2,I)*U(1,I)
    A(2)   = A(2)+U(2,I)*F(I)
  END DO
  G(2) = G(2)/S(2)
  H(2) = H(2)/S(1)
  A(2) = A(2)/S(2)
!
! Higher order polynomials u_k from the recursive relation
!
  IF(M.GE.3) THEN
    DO I = 2, M-1
      DO J = 1, N
        U(I+1,J) = X(J)*U(I,J)-G(I)*U(I,J)-H(I)*U(I-1,J)
        S(I+1)   = S(I+1) + U(I+1,J)**2
        G(I+1)   = G(I+1) + X(J)*U(I+1,J)**2
        H(I+1)   = H(I+1) + X(J)*U(I+1,J)*U(I,J)
        A(I+1)   = A(I+1) + U(I+1,J)*F(J)
      END DO
      G(I+1) = G(I+1)/S(I+1)
      H(I+1) = H(I+1)/S(I)
      A(I+1) = A(I+1)/S(I+1)
    END DO
  END IF
END SUBROUTINE PFIT
