
program main
  use data, only: cube, prueba => prueba
  implicit none

  integer, Parameter :: N = 32 ! N >= 1
  REAL :: x, y, z
  integer :: i, j

  Real, Parameter :: d = 2.0 ! length of the edge (units of length)


  ! Number of points to cover the surface of the bipyramid
  print'(I5,/,"Hola")', 2*(4*N*N + 1) ! base edge + 2 * cap

  ! Perimeter of the base: compute the points on one side and rotate it 3 times.
  do i=-N, N-1

     x = i*d/N
     z = cube(x, d)  ! if d != +-2, annular distortion of the edge.

     print 200, "H ",  x,  d,  z !
     print 200, "C ",  d, -x,  z ! Rotate -pi/2
     print 200, "N ", -x, -d,  z ! Rotate -pi
     print 200, "O ", -d,  x,  z ! Rotate -3pi/2

  end do

  do i=-N+1, N-1
     x = i*d/N
     print 100, x,  d,  cube(x, d)
     print 100, x, -d,  cube(x,-d)
     do j = -N+1, N-1
        y = j*d/N
        print 100, x, y,  cube(x,y)
        print 100, x, y, -cube(x,y) + 2*d
     end do
  end do


  ! BTA: test the 'prueba' function
  write(13, FMT='(I5,/,A)') 2*(2*N + 1)*(2*N + 1) + 1, "Hola cubo"
  write(13, FMT='(A)') 'Li 0. 0. 0.'
  do i=-N, N
     x = i*d/N
     do j = -N, N
        y = j*d/N
        write(13,FMT=200) "C", x, y,  prueba(x,y)
        write(13,FMT=200) "N", x, y, -prueba(x,y) + 2*sqrt(2.0)*d
     end do
  end do


100 FORMAT("H ", 3F12.5)
200 FORMAT(A2, 3F12.5)

end program main
