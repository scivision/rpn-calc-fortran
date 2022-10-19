module hyper

use assert, only: wp
use global

implicit none

private

interface sech
procedure sech_r, sech_c
end interface sech

interface asech
procedure asech_r, asech_c
end interface asech

interface csch
procedure csch_r, csch_c
end interface csch

interface acsch
procedure acsch_r, acsch_c
end interface acsch

interface coth
procedure coth_r, coth_c
end interface coth

interface acoth
procedure acoth_r, acoth_c
end interface acoth

public :: sech, asech, csch, acsch, coth, acoth, tanhc, ctanhc

contains


!***********************************************************************************************************************************
!  TANHC
!
!  Tanhc function.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION TANHC (X) RESULT (Y)

real(wp), INTENT(IN) :: X

IF (X .EQ. 0._wp) THEN
   Y = 1
ELSE
   Y = TANH(X) / X
END IF


END FUNCTION TANHC


!***********************************************************************************************************************************
!  CTANHC
!
!  Complex tanhc function.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CTANHC (Z) RESULT (Y)

COMPLEX(wp), INTENT(IN) :: Z

IF (Z .EQ. (0._wp, 0._wp)) THEN
   Y = (1._wp, 0._wp)
ELSE
   Y = TANH(Z) / Z
END IF

END FUNCTION CTANHC

!***********************************************************************************************************************************
!  SECH
!
!  Hyperbolic secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION SECH_r(X) result(sech)
real(wp), INTENT (IN) :: X

sech = 1/COSH(X)
END FUNCTION SECH_r


elemental complex(wp) FUNCTION SECH_c(Z) RESULT(sech)
COMPLEX(wp), INTENT (IN) :: Z

sech = 1/cosh(Z)
END FUNCTION SECH_c



!***********************************************************************************************************************************
!  ASECH
!
!  Inverse hyperbolic secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ASECH_r(y) result(asech)
real(wp), INTENT (IN) :: Y

asech = ACOSH(1/Y)
END FUNCTION ASECH_r


elemental complex(wp) FUNCTION ASECH_c(Y) result(asech)
COMPLEX(wp), INTENT (IN) :: Y

asech = ACOSH(1/Y)

END FUNCTION ASECH_c

!***********************************************************************************************************************************
!  CSCH
!
!  Hyperbolic cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CSCH_r(X) RESULT (Y)
real(wp), INTENT (IN) :: X

Y = 1/SINH(X)
END FUNCTION CSCH_r


elemental complex(wp) FUNCTION CSCH_c(Z) RESULT (Y)
COMPLEX(wp), INTENT (IN) :: Z

Y = 1 / SINH(Z)
END FUNCTION CSCH_c

!***********************************************************************************************************************************
!  ACSCH
!
!  Inverse hyperbolic cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACSCH_r(Y) RESULT (X)
real(wp), INTENT (IN) :: Y

X = ASINH(1/Y)
END FUNCTION ACSCH_r


elemental complex(wp) FUNCTION ACSCH_c(Y) RESULT (X)
COMPLEX(wp), INTENT (IN) :: Y

X = ASINH(1/Y)

END FUNCTION ACSCH_c


!***********************************************************************************************************************************
!  COTH
!
!  Hyperbolic cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION COTH_r(X) result(coth)
real(wp), INTENT (IN) :: X

coth = 1/TANH(X)
END FUNCTION COTH_r


elemental complex(wp) FUNCTION COTH_c(Z) result(coth)
COMPLEX(wp), INTENT (IN) :: Z

COTH = 1 / tanh(Z)
END FUNCTION COTH_c



!***********************************************************************************************************************************
!  ACOTH
!
!  Inverse hyperbolic cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACOTH_r(Y) result(acoth)
real(wp), INTENT (IN) :: Y

ACOTH = ATANH(1/Y)
END FUNCTION ACOTH_r


elemental complex(wp) FUNCTION ACOTH_c(Z) result(acoth)
COMPLEX(wp), INTENT(IN) :: Z

acoth = 0.5_wp*LOG((Z+1)/(Z-1))
END FUNCTION ACOTH_c


end module hyper
