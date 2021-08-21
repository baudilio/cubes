MODULE data
  implicit none
  private

  public :: cube, prueba, prueba2

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

  ! BTA: test function |z| = a - ( |x| + |y| )
  real function prueba(x,y) result(z)
    implicit none
    real, Intent(in) :: x, y
    Real :: u, v

    u = abs(x)
    v = abs(y)
    z = 0.5*sqrt(2.0)* (u + v)

    return
  end function prueba

  ! BTA: test function |z| = a - ( |x+y| + |x-y| )
  real function prueba2(x,y) result(z)
    implicit none
    real, Intent(in) :: x, y
    Real :: u, v

    u = abs(x+y)
    v = abs(x-y)
    z = 0.0 - (u + v)
    z = 0.5*sqrt(2.0) * (u + v)

    return
  end function prueba2

end MODULE data
