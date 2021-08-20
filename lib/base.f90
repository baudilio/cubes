MODULE data
  implicit none
  private

  public :: cube

contains

  real function cube(x,y) result(z)
    implicit none
    real, Intent(in) :: x, y
    Real :: u, v

    u = abs(x)
    v = abs(y)
    z = 0.5 * ( u + v + abs(u - v) )

    return
  end function cube

end MODULE data
