program trapezoidal_rule
  

  use real_type_mod, only: dp=>double_precision
  use problem_mod, only: integrand, limits
  implicit none
  
  real (kind=dp) :: x  ! x coordintate
  real (kind=dp) :: dx ! Delta x
  real (kind=dp) :: int_area ! Area of subinterval
  real (kind=dp) :: sum ! Sum of subinterval area
  real (kind=dp) :: xlow ! Lowerbound
  real (kind=dp) :: xhi ! Upper bound
  
  integer :: i            ! Loop index
  integer :: n            ! # of sub intervals

  call limits(xlow,xhi)  ! Get limits of integration

  write(*,*) "Enter number of subintervals:"
  read(*,*) n

  x = xlow   ! Initialize x to left endpoint of integration
  dx = (xhi-xlow)/n  ! Calculate Delta x
  sum = 0.0d0   ! Initialize sum

  do i=1,n

     x = (i-1)*dx + xlow
     int_area = 0.5d0*(integrand(x)+integrand(x+dx))*dx

     sum = sum + int_area
  enddo

  write(*,*) "Value of integral =", sum

  
  stop 0
end program trapezoidal_rule
