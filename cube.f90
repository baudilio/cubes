
program main
  use data, only: cube, prueba => prueba
  implicit none

  integer, Parameter :: N = 16 ! N >= 1
  REAL :: x, y, z
  integer :: i, j

  Real, Parameter :: d = 0.707106781185 ! length of the edge (units of length)
  Real, Parameter :: sq = 0.707106781185 ! sqrt(2)/2

  Integer :: lu

  ! ---

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
        z= prueba(x,y)

        write(13,FMT=200) "C", x, y,  z
        ! write(13,FMT=200) "N", x, y, -z + 2*sqrt(2.0)*d
        write(13,FMT=200) "N", sq*(x-y), sq*(x+y), -z + 2*d ! Rot pi/4 + translation
     end do
  end do

  ! BTA: square + T + (R(pi/2)x + R(pi/2)y + R(pi/2)z)
  ! BTA: T can also be regarded as a R(pi)[x,y or z]
  write(14, FMT='(I5,/,A)') 2*(2*N + 1)*(2*N + 1) + 1, "Hola cubo"
  write(14, FMT='(A)') 'Li 0. 0. 0.'
  do i=-N, N-1
     x = i*d/N
     do j = -N, N
        y = j*d/N
        z= -d

        write(14,FMT=200) "C", x, y,  z
        write(14,FMT=200) "N", x, y, -z ! T
        write(14,FMT=200) "O", x, z, -y ! Rx
        write(14,FMT=200) "N", x, -z,  y ! T


     end do
  end do


  ! Edges of a cube
  open(newunit=lu, file='edge.xyz', action='write')
  write(lu, FMT='(I6,/,"Number of points on the edges of a cube")') 4*(6*N - 1)
  do i= -N, N-1
     x = i*0.5*d/N
     y = -d*0.5
     z = -d*0.5
     write(lu, 200)     "H", x,  y,  z
     write(lu, FMT=200) "C", x,  -y, z ! T
     write(lu, FMT=200) "N", x,  y, -z ! Rx
     write(lu, FMT=200) "O", -x, -y,  -z ! T

  end do
  close(lu)

  ! Edges of a cube using quaternions
  open(newunit=lu, file='Qedge.xyz', action='write')
  write(lu, FMT='(I6,/,"Number of points on the edges of a cube")') 8 ! 4*(6*N - 1)

  x=1.;y=1.;z=1. ! A
  write(lu, FMT=200) "O",  x, y, z ! E(A)
  write(lu, FMT=200) "O", -x, -y, -z ! i(A)

  x=1.;y=1.;z=-1. ! B
  write(lu, FMT=200) "O", x, y, z ! E(B)
  write(lu, FMT=200) "O", -x, -y, -z ! i(B)

  write(lu, FMT=200) "O", z, x, y ! C3(B)
  write(lu, FMT=200) "O", -z, -x, -y ! C3[i(B)]
  write(lu, FMT=200) "O", y, z, x ! C3[C3(B)]
  write(lu, FMT=200) "O", -y, -z, -x ! C3[C3[i(B)]]

  close(lu)



100 FORMAT("H ", 3F12.5)
200 FORMAT(A2, 3F12.5)

end program main
