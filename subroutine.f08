module problem_mod
  implicit none
  contains


    subroutine limits(xlow,xhi)

      use real_type_mod, only: dp=>double_precision
      implicit none

      real(kind=dp),intent(out) :: xlow
      real(kind=dp),intent(out) :: xhi
      xlow = 1.0d0
      xhi = 3.0d0

      return
    end subroutine limits

    function integrand(x) result(f_of_x)

      use real_type_mod, only: dp=>double_precision
      implicit none

  
      real(kind=dp), intent(in) :: x
      real(kind=dp) :: f_of_x
      f_of_x = exp(-x)/x

      return
    end function integrand


end module problem_mod
