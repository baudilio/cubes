
program main
  use data, only: cube
  implicit none

  integer, Parameter :: N = 5
  REAL :: x, y
  integer :: i, j
  Real, Parameter :: d = 2.0 ! length of the edge (u. of l.)


!  print'(I5,/,"Hola")', (2*N + 1) * (2*N + 1)
!  do i=-N, N
!     x = i*d/N
!     do j = -N, N
!        y = j*d/N
!        print 100, x, y,  cube(x,y)
!     end do
!  end do

  print'(I5,/,"Hola")', (2*N + 1) * (2*N + 1)
  !print'(I5,/,"Hola")', 2*(4*N*N + 1)
  do i=-N, N
     y = i*d/N
     print 100,  d,  y,  cube( d, y)
     print 100, -d,  y,  cube(-d, y)
  end do

  do i=-N+1, N-1
     x = i*d/N
     print 100, x,  d,  cube(x, d)
     print 100, x, -d,  cube(x,-d)
     do j = -N+1, N-1
        y = j*d/N
        print 100, x, y,  cube(x,y)
     end do
  end do

  100 FORMAT("H ", 3F12.5)

end program main
