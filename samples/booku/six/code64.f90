!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 6.4   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM N_T_FIELD
!
! This program solves the time dependent temperature field
! around a nuclear waste rod in a two-dimensional model.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER, unit(m1) :: M1=1001
  INTEGER, PARAMETER, unit(m) :: M=2001
  INTEGER, PARAMETER :: N=19
  INTEGER :: I,J
  REAL :: TC,RB,H,H2,S0,CS,B0,C0,R,T
  real, unit(h) :: HT
  REAL, unit(x) :: XK
  REAL, unit(r) :: RA
  REAL, unit(t) :: T0
  REAL, unit(a) :: A0
  REAL, DIMENSION (N) :: A,B,C,Y,G,W,V,U
  REAL, DIMENSION (M,N) :: S
  REAL, DIMENSION (M,N+1) :: X
!
  HT  = 1.0/(M1-1)
  TC  = 1.0
  XK  = 3153.6
  RA  = 25.0
  RB  = 100.0
  H   = RB/(N+1)
  H2  = H*H
  T0  = 10.0
  S0  = HT*XK*T0/RA**2
  CS  = HT*XK/H2
!
  DO I = 1, M
    DO J = 1, N+1
      X(I,J) = 0.0
    END DO
  END DO
!
  DO I = 1, N
    R = H*I
    A(I) =  2.0*(1.0+CS)*R
    B(I) = -(1.0+0.5/I)*CS*R
    C(I) = -(1.0-0.5/I)*CS*R
    IF (R.LT.RA) THEN
      S(1,I) = S0*R
    ELSE
      S(1,I) = 0.0
    END IF
  END DO
!
! Assign the source of the radiation heat
!
  DO I = 2, M
    T = HT*(I-1)
    DO J = 1, N
      R = H*J
      IF (R.LT.RA) THEN
        S(I,J) = R*S0/EXP(T/TC)
      ELSE
        S(I,J) = 0.0
      END IF
    END DO
  END DO
!
  DO I = 2, M
!
! Find the values from the last time step
!
    A0 = 2.0*(1.0-CS)*H
    B0 = (1.0+0.5)*CS*H
    C0 = (1.0-0.5)*CS*H
    G(1) = A0*X(I-1,1)+B0*X(I-1,2) &
     +C0*(4.0*X(I-1,1)-X(I-1,2))/3.0+S(I-1,1)+S(I,1)
    DO J = 2, N
      R  = J*H
      A0 = 2.0*(1.0-CS)*R
      B0 = (1.0+0.5/J)*CS*R
      C0 = (1.0-0.5/J)*CS*R
      G(J) = A0*X(I-1,J)+B0*X(I-1,J+1)+C0*X(I-1,J-1)+(S(I-1,J)+S(I,J))
    END DO
!
! Find the elements in L and U
!
    W(1) = A(1)
    V(1) = C(1)
    U(1) = B(1)/W(1)
    DO J = 2, N
      V(J) = C(J)
      W(J) = A(J)-V(J-1)*U(J-1)
      U(J) = B(J)/W(J)
    END DO
!
! Find the solution of the temperature
!
    Y(1) = G(1)/W(1)
    DO J = 2, N
      Y(J) = (G(J)-V(J-1)*Y(J-1))/W(J)
    END DO
!
    X(I,N) = Y(N)
    DO J = N-1,1,-1
      X(I,J) = Y(J)-U(J)*X(I,J+1)
    END DO
  END DO
!
  I = M
  DO J = 1, N+1
    R = J*H
    WRITE (6,"(2F16.8)") R,X(I,J)
  END DO
END PROGRAM N_T_FIELD
