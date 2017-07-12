!
! Numerical Mathematics and Computing, Fifth Edition
! Ward Cheney & David Kincaid
! Brooks/Cole Publ. Co.
! Copyright (c) 2003.  All rights reserved.
! For educational use with the Cheney-Kincaid textbook.
! Absolutely no warranty implied or expressed.
!
! Section 15.1
!
! File: parabolic2.f90
!
! Parabolic pde problem solved by Crank-Nicolson method (tri)

program parabolic2
      integer, parameter :: n = 10, m = 20
      real, parameter :: h = 0.1, k = 0.005
      real, dimension (n-1) :: c,d,u,v
      integer:: i,j
      real::pi,pi2,s,r,t

      interface                                                       
      subroutine tri(n,a,d,c,b,x)                               
      integer, intent(in)::n                                    
      real, dimension(:), intent(in):: a,c
      real, dimension(:) :: d,b      
      real, dimension(:), intent(out)::x                        
      end subroutine tri                                        
      end interface

      pi = 4.0*atan(1.0)    
      pi2 = pi*pi 
      s = h*h/k  
      r = 2.0 + s 
      do i = 1,n-1
        d(i) = r  
        c(i) = -1.0 
        u(i) = sin(pi*real(i)*h)      
     end do
      print "(//(5(5x,e22.14)))",(u(i),i = 1,n-1)
      do j = 1,m 
       do i = 1,n-1
          d(i) = r
          != stencil readOnce, pointed(dim=1) :: u
          v(i) = s*u(i)     
        end do 
        call tri(n-1,c,d,c,v,v) 
        print*, "print the calculated v(i) where j = ",j
        print "(//(5(5x,e22.14)))",(v(i),i = 1,n-1)
        t = real(j)*k      
        do i = 1,n-1
           != stencil readOnce, pointed(dim=1) :: v
           u(i) = exp(-pi2*t)*sin(pi*real(i)*h) - v(i)       
        end do 
        print*, "This is the difference between true value and v(i) and j =",j
        print "(//(5(5x,e22.14)))",(u(i),i = 1,n-1)
        do i = 1,n-1
           != stencil readOnce, pointed(dim=1) :: v
           u(i) = v(i)       
        end do 
      end do 
end program parabolic2
  
subroutine tri(n,a,d,c,b,x)                                        
      integer, intent(in)::n                                       
      real, dimension(:), intent(in)::a,c
      real, dimension(:) :: d, b
      real, dimension(:), intent(out):: x                          
      integer ::i                                                  
      real :: xmult                                                
      do i = 2,n                                                   
        xmult = a(i-1)/d(i-1)                                      
        != stencil readOnce, backward(depth=1, dim=1, nonpointed) :: a, c
        != stencil readOnce, backward(depth=1, dim=1) :: d
        d(i) = d(i) - xmult*c(i-1)                                 
        != stencil readOnce, backward(depth=1, dim=1, nonpointed) :: a
        != stencil readOnce, backward(depth=1, dim=1) :: b
        != stencil readOnce, backward(depth=1, dim=1, nonpointed) :: d
        b(i) = b(i) - xmult*b(i-1)                                 
      end do                                                       
      x(n) = b(n)/d(n)                                             
      do i = n-1,1,-1                                              
        != stencil readOnce, pointed(dim=1) :: b, c, d
        != stencil readOnce, forward(depth=1, dim=1, nonpointed) :: x
        x(i) = (b(i) - c(i)*x(i+1))/d(i)                           
      end do                                                       
end subroutine tri                                                 
                                                                   








                                                                   
