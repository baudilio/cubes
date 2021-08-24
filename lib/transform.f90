MODULE transform
  implicit none
  private

  type point
     real :: x, y, z
  end type point

  Real, Parameter :: PI = 3.141592653589793

  public :: i, C3

contains

  ! BTA: inversion
  type(point) function i(v) result(u)
    type (point), intent(in) :: v
    u%x = -v%x
    u%y = -v%y
    u%z = -v%z
  end function C

  ! BTA: rotate 2pi/3 around axis i+j+k
  type(point) function C3(v) result(u)
    type (point), intent(in) :: v
    u%x = v%z
    u%y = v%x
    u%z = v%y
  end function C


end MODULE transform
