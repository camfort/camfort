!!!!!!!!!!!!!!!!!!!!!!!!!!!   Program 12.5   !!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
PROGRAM TALK_0_TO_1
  INCLUDE 'mpif.h'
  INTEGER :: IRANK,IPROC,ITAG,ISEND,IRECV,IERR,IM,ID,IFINISH
  INTEGER, DIMENSION (MPI_STATUS_SIZE) :: ISTAT
  CHARACTER*40 :: HELLO
!
  ITAG  = 730
  ID    = 40
  ISEND = 0
  IRECV = 1
  CALL MPI_INIT (IERR)
  CALL MPI_COMM_RANK (MPI_COMM_WORLD,IRANK,IERR)
  CALL MPI_COMM_SIZE (MPI_COMM_WORLD,IPROC,IERR)
  PRINT*, IRANK, IPROC
  CALL MPI_BARRIER (MPI_COMM_WORLD,IERR)
  IF (IRANK.EQ.ISEND) THEN
    HELLO = 'I am process 0, who are you ?'
    IM = 29
    CALL MPI_SEND (HELLO,IM,MPI_CHARACTER,IRECV,&
                 ITAG,MPI_COMM_WORLD,IERR)
    PRINT*, 'I sent the message: ', HELLO
  ELSE IF (IRANK.EQ.IRECV) THEN
    CALL MPI_RECV (HELLO,ID,MPI_CHARACTER,ISEND, &
                   ITAG,MPI_COMM_WORLD,ISTAT,IERR)
    PRINT*, 'I got your message which is: ', HELLO
  END IF
  CALL MPI_FINALIZE(IFINISH)
END PROGRAM TALK_0_TO_1
