module assert

use, intrinsic:: iso_c_binding, only: sp=>c_float, dp=>c_double
use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
#ifndef NO_IEEE
use, intrinsic:: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
#endif

implicit none

interface isclose
  procedure isclose_r, isclose_ri, isclose_c
end interface isclose

private

integer,parameter :: wp = dp

public :: wp, isclose, assert_isclose

contains

elemental logical function isclose_r(actual, desired, rtol, atol, equal_nan) result(isclose)
! inputs
! ------
! actual: value "measured"
! desired: value "wanted"
! rtol: relative tolerance
! atol: absolute tolerance
! equal_nan: consider NaN to be equal?
!
!  rtol overrides atol when both are specified
!
! https://www.python.org/dev/peps/pep-0485/#proposed-implementation
! https://github.com/PythonCHB/close_pep/blob/master/is_close.py

real(wp), intent(in) :: actual, desired
real(wp), intent(in), optional :: rtol, atol
logical, intent(in), optional :: equal_nan

real(wp) :: r,a
logical :: n
! this is appropriate INSTEAD OF merge(), since non present values aren't defined.
r = 1e-5_wp
a = 0
n = .false.
isclose = .false.
if (present(rtol)) r = rtol
if (present(atol)) a = atol
if (present(equal_nan)) n = equal_nan

!print*,r,a,n,actual,desired

!--- sanity check
if (r < 0 .or. a < 0) error stop 'invalid tolerance parameter(s)'
!--- equal nan
#ifndef NO_IEEE
isclose = n .and. all(ieee_is_nan([actual,desired]))
if (isclose) return
!--- Inf /= Inf, unequal NaN
if (.not. all(ieee_is_finite([actual,desired]))) return
#endif
!--- floating point closeness check
isclose = abs(actual-desired) <= max(r * max(abs(actual), abs(desired)), a)

end function isclose_r


elemental logical function isclose_ri(actual, desired, rtol, atol) result(isclose)
real(wp), intent(in) :: actual
integer, intent(in) :: desired
real(wp), intent(in), optional :: rtol, atol

real(wp) :: r,a

r = 1e-5_wp
a = 0
if (present(rtol)) r = rtol
if (present(atol)) a = atol

!--- sanity check
if ((r < 0).or.(a < 0)) error stop 'invalid tolerance parameter(s)'
#ifndef NO_IEEE
if (.not.ieee_is_finite(actual)) return
#endif
!--- floating point closeness check
isclose = abs(actual-desired) <= max(r * max(abs(actual), real(abs(desired))), a)

end function isclose_ri


elemental logical function isclose_c(actual, desired, rtol, atol) result(isclose)
! inputs
! ------
! actual: value "measured"
! desired: value "wanted"
! rtol: relative tolerance
! atol: absolute tolerance
! equal_nan: consider NaN to be equal?
!
!  rtol overrides atol when both are specified
!
! https://www.python.org/dev/peps/pep-0485/#proposed-implementation
! https://github.com/PythonCHB/close_pep/blob/master/is_close.py

complex(wp), intent(in) :: actual, desired
real(wp), intent(in), optional :: rtol, atol

real(wp) :: r,a

r = 1e-5_wp
a = 0
if (present(rtol)) r = rtol
if (present(atol)) a = atol

!--- sanity check
if ((abs(r) < 0).or.(abs(a) < 0)) error stop 'impossible rel or abs tolerance'
!--- floating point closeness check
isclose = abs(actual-desired) <= max(r * max(abs(actual), abs(desired)), a)

end function isclose_c


impure elemental subroutine assert_isclose(actual, desired, rtol, atol, equal_nan, err_msg)
! inputs
! ------
! actual: value "measured"
! desired: value "wanted"
! rtol: relative tolerance
! atol: absolute tolerance
! equal_nan: consider NaN to be equal?
! err_msg: message to print on mismatch
!
! rtol overrides atol when both are specified

real(wp), intent(in) :: actual, desired
real(wp), intent(in), optional :: rtol, atol
logical, intent(in), optional :: equal_nan
character(*), intent(in), optional :: err_msg

character(:), allocatable :: msg

if (isclose(actual,desired,rtol,atol,equal_nan)) return

msg = 'MISMATCH'
if (present(err_msg)) msg = err_msg

write(stderr,*) msg,': actual',actual,'desired',desired

error stop

end subroutine assert_isclose


end module assert
