module crossModuleProvider
  implicit none
  contains
    function add(a, b)
      implicit none
      integer :: a, b, add
      add = a + b
    end function
end module crossModuleProvider
