! Name: Adam_Fletcher
! Date: 04/20/2022
! Purpose: To calculate the definite integral using Simpson's Rule.
!          Based on the convergence table below, I think that the most accurate
!          value of the integral is 993.6316686676. The nature of the integral
!          causes an IEEE_Denormal flag which results in a loss in precision
!          in the last few digits, which is why I drop the last 4 digits.
!          The more subintervals I used, the more precise the number came to be.
!          Up to one million subintervals the value was precise to the value
!          stated.

!          Convergence Test Results
!       # Subintervals        Value of Integral
!       10                    2148.7117280943244 
!       100                   1023.9410210478311
!       110                   978.42492536419263
!       120                   999.87661526314776
!       130                   991.76179365130463
!       140                   993.81537947000129
!       150                   993.87888751250375
!       200                   993.62370862000739
!       500                   993.63166869224335
!       1000                  993.63166866915776
!       10,000                993.63166866761981
!       100,000               993.63166866761753
!       1,000,000             993.63166866763584


! =======================================================
! Begin Double Precision Module

module double_precision
  implicit none
  
                                  ! Variable Dictionary 1
  
integer, parameter :: double_prec=kind(0.0d0) ! Declare double precision var
  
end module double_precision



! =======================================================
! Begin Simpson Module and Limits Subroutine

module simpsons_module
  implicit none
contains

  subroutine limits(xlow,xhi)             ! Begin subroutine "Limits"

    use double_precision, only: dp=>double_prec
    implicit none

                               ! Variable Dictionary 2
    
    real(kind=dp),intent(out) :: xlow     ! Lower bound of integral
    real(kind=dp),intent(out) :: xhi      ! Upper bound of integral



    xlow = 1.0d0                          ! Initialize upper and lower bounds
    xhi = 20.0d0

    return
  end subroutine limits

  function integrand(x) result(f_of_x)    ! Use integral function with result

    use double_precision, only: dp=>double_prec
    implicit none

    real(kind=dp) :: a=4000               ! Declare and initialize constants    &
    real(kind=dp) :: b=15.15              ! within integral function
    real(kind=dp) :: c=0.01
     

    real(kind=dp),intent(in) :: x         ! Parameter variable of integral
    real(kind=dp) :: f_of_x               ! Result of integral

    f_of_x = (x+cos(x))*exp(cos(x))+a*(exp(-(x-b)**2/c))
    
                                          ! Integral function
    return
  end function integrand

end module simpsons_module


! =======================================================
! Begin Simpson's Rule Program


program simpsons_rule

  use double_precision, only: dp=>double_prec
  use simpsons_module, only: integrand, limits
  implicit none

                    ! Variable Dictionary 3

  real(kind=dp) :: x         ! x-coordinate
  real(kind=dp) :: dx        ! Delta x
  real(kind=dp) :: int_area  ! Area of the subinterval
  real(kind=dp) :: sum       ! Sum of subinterval area
  real(kind=dp) :: xlow      ! Lower bound of integral
  real(kind=dp) :: xhi       ! Upper bound of integral

  integer :: i               ! Loop index
  integer :: N               ! Number of subintervals

  call limits(xlow,xhi)      ! Get limits of integration from subroutine

  write(*,*) "Enter the number of subintervals:"
  read(*,*) N

  x = xlow                   ! Initialize x to left endpoint
  dx = (xhi-xlow)/n          ! Calculate delta x
  sum = 0.0d0                ! Initialize sum of area

  do i=1,N                   ! Loop over subintervals

     x = (i-1)*dx + xlow
     int_area =0.1666667d0*(integrand(x)+(4*integrand(x+(0.5d0*dx)))+          &
          (integrand(x+dx)))*dx
    
     
     sum = sum + int_area    ! Sum the areas of each interval under curve

  enddo

  write(*,*) "Value of integral =", sum

  stop 0
end program simpsons_rule






