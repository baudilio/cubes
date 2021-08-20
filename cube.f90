
program main
  use data, only: cube
  implicit none

  integer, Parameter :: N = 5
  REAL :: x, y
  integer :: i, j
  Real, Parameter :: d = 2.0 ! length of the edge (u. of l.)


  print'(I5,/,"Hola")', (2*N + 1) * (2*N + 1)
  do i=-N, N
     x = i*d/N
     do j = -N, N
        y = j*d/N
        print 100, x, y,  cube(x,y)
     end do
  end do

  100 FORMAT("H ", 3F12.5)

end program main
