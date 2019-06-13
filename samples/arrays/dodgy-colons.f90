program dodgycolons
  INTEGER, PARAMETER :: nx = 10, ny = 15, nz = 20
  REAL :: a(nx,ny,nz)
  REAL :: b(nx,ny,nz)
  REAL :: c(nx,ny,nz)
  REAL :: tmp(2)

  !This is legal, but against our style rules.
  !We prefer colons to remind people it's an array operations
  c = a + b

  !This is OK, although we would prefer loops to allow low level OMP or compiler directives
  c(:,:,:) = a(:,:,:) + b(:,:,:)

  !This is OK. Imagine OMP and/or compiler directives
  DO k = 1, nz
     DO j = 1, ny
        DO i = 1, nz
           c(i,j,k) = a(i,j,k) + b(i,j,k)
        END DO
     END DO
  END DO

  !This is wrong: i,j error
  DO k = 1, nz
     DO j = 1, ny
        DO i = 1, nz
           c(i,j,k) = a(j,i,k) + b(i,j,k)
        END DO
     END DO
  END DO

  !This is wrong- accidental array operation
  DO k = 1, nz
     DO j = 1, ny
        DO i = 1, nz
           c(:,j,k) = a(:,j,k) + b(:,j,k)
        END DO
     END DO
  END DO

  !This is OK- working on slices
  !You have to do it this way for some intrinsics (eg TRANSFER)
  DO k = 1, nz
     DO j = 1, ny
        c(:,j,k) = a(:,j,k) + b(:,j,k)
     END DO
  END DO

  !This is OK- working on slices with intrinsics
  DO k = 1, nz
     DO j = 1, ny
        DO i = 1, nz
           c(i,j,k) = a(i,j,k) + sum(b(:,j,k))
        END DO
     END DO
  END DO

  !This is OK if weird: control-flow in the inner loop uses i
  DO k = 1, nz
     DO j = 1, ny
        DO i = 1, nz
           select case(i)
           case(1)
              c(:,j,k) = a(:,j,k) + b(:,j,k)
           end select
        END DO
     END DO
  END DO

  !This is OK if weird: control-flow in the inner loop uses i
  DO k = 1, nz
     DO j = 1, ny
        DO i = 1, nz
           if(i==1) then
              c(:,j,k) = a(:,j,k) + b(:,j,k)
           endif
        END DO
     END DO
  END DO

end program dodgycolons
