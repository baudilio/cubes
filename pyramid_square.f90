program main
   implicit none

   integer, Parameter :: N = 13
   real :: x, y, z = .0
   real :: xp, yp
   integer :: i, j
   real :: a

   Real, Parameter :: r = 1.0/sqrt(2.0)

   ! ---

   a = 2*r/(N - 1)

   write (13, FMT="(I5,/,'Pyramid of squared base.')") 2*N*N
   do i = 1, N
      x = a*(i - 1) - r
      do j = 1, N
         y = a*(j - 1) - r

         ! write (13, 100) x, y, z

         xp = r*(x - y)
         yp = r*(x + y)
         z = 1.0 - (abs(xp) + abs(yp))

         write (13, 100) xp, yp, z
         write (13, 100) xp, yp, -z
      end do
   end do

100 format("H ", 3F12.6)
   stop 0;
end program main
