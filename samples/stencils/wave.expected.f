      program wave
      implicit none

      integer i, k, ni, nk
      parameter (ni = 3, nk = 3)
      real dt,dx,v,a,b1,dp
      
      real p(0:ni,0:nk),p0(0:ni,0:nk)

! compute the constant coeffect of the stiffness matrix
      b1 = dt/dx*v
      a = b1*b1

! First inital conditiona and boundary conditions
! Left boundary
      p(0,0) = 0.0
! Time iteration -1 = space loop.
      do 1 i = 1, ni
         p(i,0) = p0(i*dx)
 1    continue
! Right boundary
      p(ni,0) = 0.0

! Second initial condition (with p1=0) and boundary conditions
! Left boundary
      p(0,1) = 0.0
! Time iteration 0 = space loop.
      do 2 i = 1, ni
         dp = p(i+1,0) - 2*p(i,0) + p(i-1,0)
c= stencil (centered(depth=1, dim=1)) :: p
         p(i,1) = p(i,0) + 0.5*a*dp
 2    continue
! Right boundary
      p(ni,1) = 0.0

! Evolution problem and boundary conditions

      do 3 k = 1, nk
! Left boundary
         p(0, k+1) = 0
! Time iteration k = space loop
         do 4 i = 1, ni
            dp = p(i+1,k) - 2*p(i,k) + p(i-1,k)
c= stencil (pointed(dim=1))*(backward(depth=2, dim=2, nonpointed)) + (centered(depth=1, dim=1))*(backward(depth=1, dim=2, nonpointed)) :: p
            p(i,k+1) = 2*p(i,k) - p(i,k-1) + a*dp
 4       continue
! Right boundary
      p(ni,k+1) = 0.0
 3    continue
      end

! *Main> stencilsInf "samples/stencils/wave.f" [] () ()
! Inferring stencil specs for "samples/stencils/wave.f"

! samples/stencils/wave.f
! Output of the analysis:
! ((19,8),(19,26)) 	p0: unspecified 
! ((29,8),(29,44)) 	p: centered depth=1 dim=0,fixed dim=1
! ((30,8),(30,35)) 	p: reflexive dims=0,fixed dim=1
! ((42,8),(42,47)) 	p: centered depth=1 dim=0,reflexive dims=1
! ((43,8),(43,49)) 	p: reflexive dims=0,backward depth=1 dim=1
