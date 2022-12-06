program main
   implicit none

   integer :: i, j, k
   integer, parameter :: n = 8
   real :: x, y, z
   real :: a

   a = 2.0/(n - 1)

   write (13, FMT="(I5,/,'Dale')") 2*n*n + 2*(n*n - 2*n) + 2*(n*n - 2*n - 2*(n - 2))
   do i = 1, n
      x = a*(i - 1) - 1
      do j = 1, n
         y = a*(j - 1) - 1
         do k = 1, n
            z = a*(k - 1) - 1

            ! write (12, FMT='("  ", 3I4, " - ", 3F12.6)') i, j, k, x, y, z

            if (max(abs(x), abs(y), abs(z)) == 1.0) then
               write (13, 100) x, y, z
            end if

         end do
      end do
   end do

100 format("H ", 3F12.6)
   stop 0
end program main
