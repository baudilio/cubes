MODULE transform
  implicit none

  type point
     real :: x, y, z
  end type point

  Real, Parameter :: PI = 3.141592653589793

  public :: invert, C3


contains

  ! BTA: inversion
  type(point) function invert(v) result(u)
    type (point), intent(in) :: v
    u%x = -v%x
    u%y = -v%y
    u%z = -v%z
  end function invert

  ! BTA: rotate 2pi/3 around axis i+j+k
  type(point) function C3(v) result(u)
    type (point), intent(in) :: v
    u%x = v%z
    u%y = v%x
    u%z = v%y
  end function C3


end MODULE transform
