module implicitnone
  implicit none

  interface
     subroutine s()
       ! separate scope, implicit none from above does not apply within interface block
     end subroutine s
  end interface

end module implicitnone
