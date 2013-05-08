module simulation
  use helpers
  implicit none

  contains

    subroutine compute_tentative_velocity(u, v, f, g, flag, del_t)
      real u(0:imax+1, 0:jmax+1), v(0:imax+1, 0:jmax+1), f(0:imax+1, 0:jmax+1), g(0:imax+1, 0:jmax+1)
      integer flag(0:imax+1, 0:jmax+1)
      real, intent(in) :: del_t

      integer i, j
      real du2dx, duvdy, duvdx, dv2dy, laplu, laplv

      do i = 1, (imax-1)
        do j = 1, jmax
            ! only if both adjacent cells are fluid cells */
            if (toLogical(iand(flag(i,j), C_F)) .and. toLogical(iand(flag(i+1,j), C_F))) then
               
                du2dx = ((u(i,j)+u(i+1,j))*(u(i,j)+u(i+1,j))+ & 
			   gamma*abs(u(i,j)+u(i+1,j))*(u(i,j)-u(i+1,j))- &
                        (u(i-1,j)+u(i,j))*(u(i-1,j)+u(i,j))- & ! insert bug #1 here u(i-1,j) to u(i, j)
			   gamma*abs(u(i-1,j)+u(i,j))*(u(i-1,j)-u(i,j))) & ! insert bug #2 here delete last term *(u(i-1,j)-u(i,j))
                         /(4.0*delx)
                duvdy = ((v(i,j)+v(i+1,j))*(u(i,j)+u(i,j+1))+ &
                           gamma*abs(v(i,j)+v(i+1,j))*(u(i,j)-u(i,j+1))- &
                         (v(i,j-1)+v(i+1,j-1))*(u(i,j-1)+u(i,j))-  &
                           gamma*abs(v(i,j-1)+v(i+1,j-1))*(u(i,j-1)-u(i,j))) &
                         /(4.0*dely)
                laplu = (u(i+1,j)-2.0*u(i,j)+u(i-1,j))/delx/delx+ &
                        (u(i,j+1)-2.0*u(i,j)+u(i,j-1))/dely/dely
   
                f(i,j) = u(i,j)+del_t*(laplu/Re-du2dx-duvdy)
            else 
                f(i,j) = u(i,j)
            end if
        end do
    end do

    do i = 1, imax
       do j = 1, (jmax-1)
          ! only if both adjacent cells are fluid cells
          if (toLogical(iand(flag(i,j), C_F)) .and. toLogical(iand(flag(i,j+1), C_F))) then
             
             duvdx = ((u(i,j)+u(i,j+1))*(v(i,j)+v(i+1,j))+  &
                      gamma*abs(u(i,j)+u(i,j+1))*(v(i,j)-v(i+1,j))- &
                      (u(i-1,j)+u(i-1,j+1))*(v(i-1,j)+v(i,j))- &
                      gamma*abs(u(i-1,j)+u(i-1,j+1))*(v(i-1,j)-v(i,j))) &
                      /(4.0*delx)
             dv2dy = ((v(i,j)+v(i,j+1))*(v(i,j)+v(i,j+1))+ &
                      gamma*abs(v(i,j)+v(i,j+1))*(v(i,j)-v(i,j+1))- &
                      (v(i,j-1)+v(i,j))*(v(i,j-1)+v(i,j))- &
                      gamma*abs(v(i,j-1)+v(i,j))*(v(i,j-1)-v(i,j))) &
                      /(4.0*dely)

             laplv = (v(i+1,j)-2.0*v(i,j)+v(i-1,j))/delx/delx+ &
                      (v(i,j+1)-2.0*v(i,j)+v(i,j-1))/dely/dely

                g(i,j) = v(i,j)+del_t*(laplv/Re-duvdx-dv2dy)
           else
                g(i,j) = v(i,j)
           end if
        end do
    end do

    f(0, 1:jmax) = u(0, 1:jmax)
    f(imax, 1:jmax) = u(imax, 1:jmax)
    
    g(1:imax, 0) = v(1:imax, 0)
    g(1:imax, jmax) = v(1:imax, jmax)
   end subroutine 

   subroutine compute_rhs(f, g, rhs, flag, del_t)
     real rhs(0:imax+1, 0:jmax+1)
     real f(0:imax+1, 0:jmax+1), g(0:imax+1, 0:jmax+1) 
     integer flag(0:imax+1, 0:jmax+1)
     real, intent(in) :: del_t

     integer :: i, j

     do i = 1, imax
        do j = 1, jmax
           if (toLogical(iand(flag(i,j), C_F))) then
                ! only do fluid and non-surface cells */
                rhs(i,j) = ((f(i,j)-f(i-1,j))/delx + (g(i,j)-g(i,j-1))/dely) / del_t
            end if
        end do
    end do
   end subroutine

   integer function poisson(p, rhs, flag, res, ifluid) 
     integer, intent(in) :: ifluid
     real :: res
     real p(0:imax+1, 0:jmax+1), rhs(0:imax+1, 0:jmax+1) 
     integer flag(0:imax+1, 0:jmax+1)
      
     integer :: i, j, iter
     real :: resp, beta_mod, p0 = 0.0
     
     integer :: eps_E, eps_W, eps_N, eps_S

     integer :: rb
    
     real :: rdx2 = 1.0/(delx*delx)
     real :: rdy2 = 1.0/(dely*dely)
     real :: beta_2

     beta_2 = -omega/(2.0*(rdx2+rdy2))

     ! Calculate sum of squares
     do i = 1, imax
        do j = 1, jmax
           if (toLogical(iand(flag(i,j), C_F))) p0 = p0 + p(i,j)*p(i,j)
        end do
     end do
   
     p0 = sqrt(p0/ifluid)
     if (p0 < 0.0001) p0 = 1.0

     

     ! Red/Black SOR-iteration
     do iter = 0, (itermax-1)
        do rb = 0, 1, 1
           do i = 1,  imax, 1
              do j = 1, jmax, 1
                 eps_E = toMask(iand(flag(i+1,j), C_F))
                 eps_W = toMask(iand(flag(i-1,j), C_F))
                 eps_N = toMask(iand(flag(i,j+1), C_F))
                 eps_S = toMask(iand(flag(i,j-1), C_F))

                 if (mod((i+j),2) == rb) then
                    if (flag(i,j) == (ior(C_F, B_NSEW))) then
                       ! five point star for interior fluid cells
                       p(i,j) = (1.-omega)*p(i,j) -              &
                                  beta_2*(                       &
                                    (p(i+1,j)+p(i-1,j))*rdx2     &
                                    + (p(i,j+1)+p(i,j-1))*rdy2   &
                                    -  rhs(i,j))
                    else if (toLogical(iand(flag(i,j), C_F))) then
                       ! modified star near boundary
                       beta_mod = -omega/((eps_E+eps_W)*rdx2+(eps_N+eps_S)*rdy2)
                       p(i,j) = (1.-omega)*p(i,j) -                        &
                                   beta_mod*(                              &
                                   (eps_E*p(i+1,j)+eps_W*p(i-1,j))*rdx2    &
                                   + (eps_N*p(i,j+1)+eps_S*p(i,j-1))*rdy2  &
                                   - rhs(i,j))
                       
                    end if
                 end if   
              end do ! end of j
           end do ! end of i
        end do ! end of rb
           
        !Partial computation of residual
        res = 0.0
        do i = 1, imax
           do j = 1, jmax
              if (toLogical(iand(flag(i,j), C_F))) then
                 ! only fluid cells
                 eps_E = toMask(iand(flag(i+1,j), C_F))
                 eps_W = toMask(iand(flag(i-1,j), C_F))
                 eps_N = toMask(iand(flag(i,j+1), C_F))
                 eps_S = toMask(iand(flag(i,j-1), C_F))

                 resp = ((eps_E*(p(i+1,j) - p(i,j))) -                &
                         (eps_W*(p(i,j) - p(i-1,j)))) * rdx2  +       &
                        (((eps_N*(p(i,j+1) - p(i,j))) -                &
                        (eps_S*(p(i,j)-p(i,j-1)))) * rdy2)  - rhs(i,j)
                 res = res + resp*resp
              end if
           end do
        end do
        ! write (*,*) res, ifluid, p0
        res = sqrt((res)/ifluid)/p0
           
        ! convergence?
        if (res<eps) exit
           
     end do ! end of iter
     
     poisson = iter
   end function

   subroutine update_velocity(u, v, f, g, p, flag, del_t)
    real u(0:imax+1, 0:jmax+1), v(0:imax+1, 0:jmax+1), f(0:imax+1, 0:jmax+1), g(0:imax+1, 0:jmax+1), p(0:imax+1, 0:jmax+1)
    integer flag(0:imax+1, 0:jmax+1)
    real, intent(in) :: del_t
     
    integer :: i, j

    do i = 1, (imax-1)
        do j = 1, jmax
            ! only if both adjacent cells are fluid cells */
            if (toLogical(iand(flag(i,j), C_F)) .and. toLogical(iand(flag(i+1,j), C_F))) then
                u(i,j) = f(i,j)-(p(i+1,j)-p(i,j))*del_t/delx
            end if
        end do
    end do
    do i = 1, imax
        do j = 1, (jmax-1)
            ! only if both adjacent cells are fluid cells 
            if (toLogical(iand(flag(i,j), C_F)) .and. toLogical(iand(flag(i,j+1), C_F))) then
	      v(i,j) = g(i,j)-(p(i,j+1)-p(i,j))*del_t/dely
            end if
        end do
    end do
  end subroutine

    real function set_timestep_interval(u, v, del_t)
      integer :: i, j
      real, intent(in) :: del_t
      real u(0:imax+1, 0:jmax+1), v(0:imax+1, 0:jmax+1) 
      real :: umax, vmax, deltu, deltv, deltRe, del_tp

      ! del_t satisfying CFL conditions
      if (tau >= 1.0e-10) then
         umax = 1.0e-10
         vmax = 1.0e-10
         
         do i = 0, (imax+1), 1
            do j = 1, (jmax+1), 1
               umax = max(abs(u(i,j)), umax)
            end do
         end do
         do  i = 1, (imax+1), 1
            do j = 0, (jmax+1), 1
               vmax = max(abs(v(i,j)), vmax)
            end do
         end do
         
         deltu = delx/umax
         deltv = dely/vmax 
         deltRe = 1/(1/(delx*delx)+1/(dely*dely))*Re/2.0
         
         if (deltu < deltv) then
            del_tp = min(deltu, deltRe)
         else 
            del_tp = min(deltv, deltRe)
         end if
         
         !multiply by safety factor
         set_timestep_interval = tau * (del_tp)
      else
         set_timestep_interval = del_t
      end if
   end function 

end module simulation
