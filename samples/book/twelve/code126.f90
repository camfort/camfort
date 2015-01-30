!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 12.6   !!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM EULER_CONST
  INCLUDE 'mpif.h'
  INTEGER :: N,K,IERR,IRANK,IPROC,IFINISH
  REAL*8, PARAMETER :: SUM25=0.577215664901532860606512D0
  REAL*8 :: SUMI,SUM
!
  CALL MPI_INIT (IERR)
  CALL MPI_COMM_RANK (MPI_COMM_WORLD,IRANK,IERR)
  CALL MPI_COMM_SIZE (MPI_COMM_WORLD,IPROC,IERR)
!
  IF (IRANK.EQ.0) THEN
    PRINT*, 'Enter total number of terms in the series: '
    READ (5,*) N
  END IF
!
! Broadcast the total number of terms to every process
!
  CALL MPI_BCAST (N,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
  K = (N/IPROC)
  SUMI = 0.D0
!
  IF (IRANK.NE.(IPROC-1)) then
    DO I = IRANK*K+1, (IRANK+1)*K
      SUMI = SUMI+1.D0/DFLOAT(I)
    END DO
  ELSE
    DO I = IRANK*K+1, N
      SUMI = SUMI + 1.D0/DFLOAT(I)
    END DO
  END IF
!
! Collect the sums from every process
!
  CALL MPI_REDUCE (SUMI,SUM,1,MPI_DOUBLE_PRECISION, &
                   MPI_SUM,0,MPI_COMM_WORLD,IERR)
  IF (IRANK.EQ.0) THEN
    SUM = SUM-DLOG(DFLOAT(N))
    PRINT*, 'The evaluated Euler constant is ', SUM, &
            'with the estimated error of ', DABS(SUM-SUM25)
  END IF
!
  CALL MPI_FINALIZE (IFINISH)
END PROGRAM EULER_CONST
