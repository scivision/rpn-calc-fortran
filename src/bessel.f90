module bessel

use assert, only: wp, isclose
use rjb, only: rjbesl
use rji, only: ribesl
use rjk, only: rkbesl
use rjy, only: rybesl
use i0, only: besi0
use i1, only: besi1
use k0, only: besk0
use k1, only: besk1

implicit none

private
public :: jinc, besi0, besi1, besk0, besk1, ribesl, rjbesl, rkbesl, rybesl

contains


elemental real(wp) FUNCTION jinc(X)
real(wp), INTENT(IN) :: X

IF (isclose(x, 0)) THEN
   jinc = 0.5_wp
ELSE
   jinc = bessel_j1(X)/X
END IF
END FUNCTION jinc


end module bessel
