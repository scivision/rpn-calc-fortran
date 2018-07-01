program test_funcs
use funcs
use assert, only: assert_isclose, wp
implicit none

real(wp), parameter :: pi = 4._wp * atan(1._wp), pi3 = pi/3
complex(wp), parameter :: cpi = (pi, pi)
integer :: N,D


call assert_isclose(csc(acsc(pi3)), pi3)

if (.not.isdigit('3')) error stop
if (isdigit('x')) error stop

N=6; D=10
call ratnorm(N,D)
if (.not. N==3 .and. D==5) error stop

call assert_isclose(frac(pi), pi-3)
call assert_isclose(frac(-pi), -pi+3)

print *,'OK RPN test'
end program
