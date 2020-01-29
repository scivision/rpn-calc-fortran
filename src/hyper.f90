module hyper
use, intrinsic:: ieee_arithmetic
use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use assert, only: wp
use rat, only: SWITCH_RAT_TO_REAL
use global

implicit none

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
contains

!***********************************************************************************************************************************
!  SECH
!
!  Hyperbolic secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION SECH_r(X) result(sech)
real(wp), INTENT (IN) :: X

sech = 1._wp/COSH(X)
END FUNCTION SECH_r


elemental complex(wp) FUNCTION SECH_c(Z) RESULT(sech)
COMPLEX(wp), INTENT (IN) :: Z

sech = 1._wp/cosh(Z)
END FUNCTION SECH_c



!***********************************************************************************************************************************
!  ASECH
!
!  Inverse hyperbolic secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ASECH_r(y) result(asech)
real(wp), INTENT (IN) :: Y

asech = ACOSH(1._wp/Y)
END FUNCTION ASECH_r


elemental complex(wp) FUNCTION ASECH_c(Y) result(asech)
COMPLEX(wp), INTENT (IN) :: Y

asech = ACOSH(1._wp/Y)

END FUNCTION ASECH_c

!***********************************************************************************************************************************
!  CSCH
!
!  Hyperbolic cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CSCH_r(X) RESULT (Y)
real(wp), INTENT (IN) :: X

Y = 1._wp/SINH(X)
END FUNCTION CSCH_r


elemental complex(wp) FUNCTION CSCH_c(Z) RESULT (Y)
COMPLEX(wp), INTENT (IN) :: Z

Y = 1._wp / SINH(Z)
END FUNCTION CSCH_c

!***********************************************************************************************************************************
!  ACSCH
!
!  Inverse hyperbolic cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACSCH_r(Y) RESULT (X)
real(wp), INTENT (IN) :: Y

X = ASINH(1._wp/Y)
END FUNCTION ACSCH_r


elemental complex(wp) FUNCTION ACSCH_c(Y) RESULT (X)
COMPLEX(wp), INTENT (IN) :: Y

X = ASINH(1._wp/Y)

END FUNCTION ACSCH_c


!***********************************************************************************************************************************
!  COTH
!
!  Hyperbolic cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION COTH_r(X) result(coth)
real(wp), INTENT (IN) :: X

coth = 1._wp/TANH(X)
END FUNCTION COTH_r


elemental complex(wp) FUNCTION COTH_c(Z) result(coth)
COMPLEX(wp), INTENT (IN) :: Z

COTH = 1._wp / tanh(Z)
END FUNCTION COTH_c



!***********************************************************************************************************************************
!  ACOTH
!
!  Inverse hyperbolic cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACOTH_r(Y) result(acoth)
real(wp), INTENT (IN) :: Y

ACOTH = ATANH(1._wp/Y)
END FUNCTION ACOTH_r


elemental complex(wp) FUNCTION ACOTH_c(Z) result(acoth)
COMPLEX(wp), INTENT(IN) :: Z

acoth = 0.5_wp*LOG((Z+1._wp)/(Z-1._wp))
END FUNCTION ACOTH_c

!-------------------------------------------------
subroutine hasin(domain_mode)
integer, intent(in) :: domain_mode

SELECT CASE (DOMAIN_MODE)
  CASE (1)
     LASTX = STACK(1)
     STACK(1) = ASINH(STACK(1))
  CASE (2)
     CLASTX = CSTACK(1)
     CSTACK(1) = ASINH(CSTACK(1))
  CASE (3)
     CALL SWITCH_RAT_TO_REAL
     LASTX = STACK(1)
     STACK(1) = ASINH(STACK(1))
END SELECT

end subroutine hasin


subroutine hacos(domain_mode)
integer, intent(in) :: domain_mode

   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) < 1._wp) THEN
            write(stderr, *) '  ACOSH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOSH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ACOSH(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) < RDSTACK(1)) THEN
            write(stderr, *) '  ACOSH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOSH(STACK(1))
         END IF
   END SELECT
end subroutine hacos


subroutine hatan(domain_mode)
integer, intent(in) :: domain_mode

SELECT CASE (DOMAIN_MODE)
  CASE (1)
     IF (ABS(STACK(1)) >= 1._wp) THEN
        write(stderr, *) '  ATANH Error'
     ELSE
        LASTX = STACK(1)
        STACK(1) = ATANH(STACK(1))
     END IF
  CASE (2)
     CLASTX = CSTACK(1)
     CSTACK(1) = ATANH(CSTACK(1))
  CASE (3)
     IF (ABS(RNSTACK(1)) >= ABS(RDSTACK(1))) THEN
        write(stderr, *) '  ATANH Error'
     ELSE
        CALL SWITCH_RAT_TO_REAL
        LASTX = STACK(1)
        STACK(1) = ATANH(STACK(1))
     END IF
END SELECT

end subroutine hatan

end module hyper
