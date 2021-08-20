
program main
  use data, only: cube
  implicit none

  integer, Parameter :: N = 24 ! N >= 1
  REAL :: x, y, z
  integer :: i, j
  Real, Parameter :: d = 2.0 ! length of the edge (units of length)


  ! Number of points to cover the surface of the bipyramid
  print'(I5,/,"Hola")', 2*(4*N*N + 1) ! base edge + 2 * cap

  ! Perimeter of the base
  do i=-N, N-1

     x = i*d/N
     z = cube(x, d)  ! if d != +-2, annular distortion of the edge.

     print 200, "H ",  x,  d,  z !
     print 200, "C ",  d, -x,  z ! Rotate -pi/2
     print 200, "N ", -x, -d,  z ! Rotate -pi
     print 200, "O ", -d,  x,  z ! Rotate -3pi/2

  end do

  ! Caps of the pyramid
  do i=-N+1, N-1
     x = i*d/N
     do j = -N+1, N-1
        y = j*d/N
        print 100, x, y,  cube(x,y) ! One cap of the pyramid.
        print 100, x, y, -cube(x,y) + 2*d ! The second cap, shifted to complete the pyramid.
     end do
  end do

  100 FORMAT("H ", 3F12.5)
  200 FORMAT(A2, 3F12.5)

end program main
