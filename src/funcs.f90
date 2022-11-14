module funcs

use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use assert, only: wp, isclose
use bessel
use trig
use rat, only: ratnorm, rdiv, rsub, radd, rmul, isfrac, lcm, gcd, isint, isreal, iscomplex, isdigit, isrational, &
  dec_to_frac, frac_to_mixed
use hyper
use stats
use fgamma

implicit none

interface cuberoot
procedure cuberoot_r, cuberoot_c
end interface cuberoot

interface frac
procedure frac_r, frac_c
end interface frac

interface sinc
procedure sinc_r, sinc_c
end interface sinc

interface tanc
procedure tanc_r, tanc_c
end interface tanc

real(wp), parameter, private :: xinf = huge(0._wp), xmax = xinf, xmin = tiny(0._wp)
complex(wp), parameter, private :: c0 = (0._wp, 0._wp)

contains

!***********************************************************************************************************************************
!  FRAC
!
!  Fractional part of a number.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION FRAC_r (X) RESULT (Y)

real(wp), INTENT(IN) :: X
real(wp) ::  Z

Z = ABS(X)
Y = Z - INT(Z)
Y = SIGN(Y,X)

END FUNCTION FRAC_r


elemental complex(wp) FUNCTION FRAC_c (X) result(r)

COMPLEX(wp), INTENT(IN) :: X
real(wp) :: XR, XI, YR, YI, ZR, ZI

XR = real(X, wp)
XI = X%IM

ZR = ABS(XR)
YR = ZR - INT(ZR)
YR = SIGN(YR,XR)

ZI = ABS(XI)
YI = ZI - INT(ZI)
YI = SIGN(YI,XI)

r = CMPLX(YR,YI, wp)

END FUNCTION FRAC_c


!***********************************************************************************************************************************
!  RFRAC
!
!  Rational FRAC.
!***********************************************************************************************************************************

elemental SUBROUTINE RFRAC (N, D, NR, DR)

INTEGER, INTENT(IN) :: N, D
INTEGER, INTENT(OUT) :: NR, DR
INTEGER :: NI, NA, DA

NA = ABS(N)
DA = ABS(D)
NI = RINT (NA, DA)
CALL RSUB (NA, DA, NI, 1, NR, DR)
NR = SIGN(NR,N)
CALL RATNORM (NR, DR)

END SUBROUTINE RFRAC


!***********************************************************************************************************************************
!  CINT
!
!  Complex INT.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CINT (X) RESULT (Y)

COMPLEX(wp), INTENT(IN) :: X

real(wp) :: YR, YI

YR = INT(real(X, wp))
YI = INT(X%IM)
Y = CMPLX(YR,YI, wp)

END FUNCTION CINT


!***********************************************************************************************************************************
!  RINT
!
!  Rational INT.
!***********************************************************************************************************************************

elemental integer FUNCTION RINT (N, D) RESULT (R)

INTEGER, INTENT(IN) :: N, D
INTEGER :: NN, DN

NN = N
DN = D
CALL RATNORM (NN, DN)
R = NN / DN

END FUNCTION RINT



!***********************************************************************************************************************************
!  RNINT
!
!  Rational NINT.
!***********************************************************************************************************************************

elemental SUBROUTINE RNINT (N, D)

INTEGER, INTENT(IN OUT) :: N, D
INTEGER :: NN, DN, TN, TD

NN = N
DN = D
CALL RATNORM (NN, DN)

IF (NN .GE. 0) THEN
   CALL RADD (NN, DN, 1, 2, TN, TD)
   N = TN / TD
   D = 1
ELSE
   CALL RSUB (NN, DN, 1, 2, TN, TD)
   N = TN / TD
   D = 1
END IF

END SUBROUTINE RNINT





!***********************************************************************************************************************************
!  CMOD
!
!  Complex MOD.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CMOD (X,Y) RESULT (Z)

COMPLEX(wp), INTENT(IN) :: X, Y

Z = X - CINT(X/Y)*Y

END FUNCTION CMOD



!***********************************************************************************************************************************
!  RMOD
!
!  Rational MOD.
!***********************************************************************************************************************************

elemental SUBROUTINE RMOD (N1, D1, N2, D2, NR, DR)

INTEGER, INTENT(IN) :: N1, D1, N2, D2
INTEGER, INTENT(OUT) :: NR, DR
INTEGER :: NAN, DAN, NBN, DBN, NT, DT, ITMP

NAN = N1
DAN = D1

NBN = N2
DBN = D2

CALL RATNORM (NAN,DAN)
CALL RATNORM (NBN,DBN)

CALL RDIV (NAN, DAN, NBN, DBN, NT, DT)
ITMP = RINT (NT, DT)
CALL RMUL (ITMP, 1, NBN, DBN, NT, DT)
CALL RSUB (NAN, DAN, NT, DT, NR, DR)
CALL RATNORM (NR, DR)

END SUBROUTINE RMOD

!***********************************************************************************************************************************
!  CUBEROOT
!
!  Computes the cube root.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CUBEROOT_r(X)  result(cuberoot)
real(wp), INTENT(IN) :: X

 CUBEROOT = SIGN((ABS(X))**(1._wp / 3._wp),X)

END FUNCTION CUBEROOT_r

elemental complex(wp) FUNCTION CUBEROOT_c (Z) result(cuberoot)
COMPLEX(wp), INTENT(IN) :: Z

 CUBEROOT = Z**(1._wp / 3._wp)
END FUNCTION CUBEROOT_c


!***********************************************************************************************************************************
!  CLOG10
!
!  Complex common logarithm.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CLOG10 (X)

COMPLEX(wp), INTENT(IN) :: X

 CLOG10 = LOG(X) / LOG(10._wp)

END FUNCTION CLOG10


elemental real(wp) FUNCTION LOG1P (X) RESULT (Y)
!  Compute log(1+x).
real(wp), INTENT(IN) :: X
real(wp) :: Z

Z = 1._wp + X
Y = LOG(Z) - ((Z-1._wp)-X) / Z                ! cancels errors with IEEE arithmetic

END FUNCTION LOG1P


elemental real(wp) FUNCTION SINC_r(X) result(sinc)
!  Sine cardinal (sinc) function.
real(wp), INTENT(IN) :: X

IF (isclose(x, 0._wp)) THEN
   sinc = 1._wp
ELSE
   sinc = SIN(X)/X
END IF
END FUNCTION SINC_r


elemental complex(wp) FUNCTION SINC_c (Z) RESULT (sinc)
!  Complex sine cardinal (sinc) function.
COMPLEX(wp), INTENT(IN) :: Z

IF (isclose(z, c0)) THEN
   sinc = (1._wp, 0._wp)
ELSE
   sinc = SIN(Z)/Z
END IF
END FUNCTION SINC_c


elemental real(wp) FUNCTION TANC_r(X) RESULT (tanc)
!  Tanc function.
real(wp), INTENT(IN) :: X

IF (isclose(x, 0._wp)) THEN
   tanc = 1._wp
ELSE
   tanc = TAN(X)/X
END IF
END FUNCTION TANC_r


elemental complex(wp) FUNCTION TANC_c (Z) RESULT (tanc)
!  Complex tanc function.
COMPLEX(wp), INTENT(IN) :: Z

IF (isclose(z, c0)) THEN
   tanc = (1._wp, 0._wp)
ELSE
   tanc = TAN(Z)/Z
END IF
END FUNCTION TANC_c


elemental real(wp) FUNCTION SINHC (X) RESULT (Y)
!  Sinhc function.
real(wp), INTENT(IN) :: X

IF (isclose(x, 0._wp)) THEN
   Y = 1._wp
ELSE
   Y = SINH(X)/X
END IF
END FUNCTION SINHC


elemental complex(wp) FUNCTION CSINHC (Z) RESULT (Y)
!  Complex sinhc function.
COMPLEX(wp), INTENT(IN) :: Z

IF (isclose(z, c0)) THEN
   Y = (1._wp,0._wp)
ELSE
   Y = SINH(Z)/Z
END IF
END FUNCTION CSINHC



!***********************************************************************************************************************************
!  ERROR FUNCTIONS
!
!  From http://www.netlib.org/specfun
!
!  DERFCX
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ERFCX(X)
real(wp), intent(in) :: X

erfcx = exp(x*x)*erfc(x)
END FUNCTION ERFCX

!***********************************************************************************************************************************
!  H2HMSD
!
!  Convert decimal hours to hours, minutes, and seconds. Seconds are returned as a real value.
!***********************************************************************************************************************************

elemental SUBROUTINE H2HMSD (DHR, IHR, IMIN, second)

real(wp), INTENT(IN) :: DHR
INTEGER, INTENT(OUT) :: IHR, IMIN
real(wp), INTENT(OUT) :: second
real(wp) :: TIME


TIME = DHR                                                                    ! hours
IHR = INT(TIME)                                                               ! hours
TIME = 60 * (TIME - IHR)                                                  ! minutes
IMIN = INT(TIME)                                                              ! minutes
second = 60 * (TIME - IMIN)                                                  ! seconds

END SUBROUTINE H2HMSD


!***********************************************************************************************************************************
!  HMS2H
!
!  Convert hours, minutes, and seconds to decimal hours.
!***********************************************************************************************************************************

elemental real(wp) function HMS2H (IHR, minute, second)

INTEGER, INTENT(IN) :: IHR, minute
real(wp), INTENT(IN) :: second

HMS2H = real(IHR) + real(minute)/60 + second/3600

END function HMS2H


!***********************************************************************************************************************************
!  RIEMANNZETA
!
!  Riemann zeta function.
!
!  Algorithm from "Atlas for Computing Mathematical Functions" by W.J. Thompson, Wiley, 1997.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION RIEMANNZETA (S,EPS)

!     Riemann zeta - 1  for  x > 1

real(wp), INTENT(IN) :: S, EPS


real(wp) :: NSTERM, SUM, FN, NEGS
INTEGER :: N,K

!     Estimate N for accuracy  eps

NSTERM = S*(S+1)*(S+2)* &
  (S+3)*(S+4)/30240
N = int((NSTERM*(2**S)/EPS)**(1 / (S+5)))
IF ( N < 10 )  THEN
   N = 10
END IF

FN = N
NEGS = -S
!     Direct sum
SUM = 0
DO K =2, N-1
   SUM = SUM+K**NEGS
END DO

!     Add Euler-Maclaurin correction terms
SUM = SUM+(FN**NEGS)*(0.5D00+FN/(S-1) &
  +S*(1 - (S + 1)*(S+2)/ &
  (60*FN*FN)) &
  /(12*FN))+NSTERM/(FN**(S+5))
riemannZETA = SUM

END FUNCTION RIEMANNZETA



!***********************************************************************************************************************************
!  REDUCE
!
!  Reduce an angle to the range [angle_min, angle_max).
!***********************************************************************************************************************************

elemental real(wp) FUNCTION REDUCE (THETA, ANGLE_MIN) RESULT (RHO)

real(wp), PARAMETER :: tau = 2*4*atan(1._wp)

real(wp), INTENT(IN) :: THETA
real(wp), INTENT(IN) :: ANGLE_MIN

real(wp) :: ANGLE_MAX
real(wp) :: REVS


!
!     Start of code.
!

ANGLE_MAX = ANGLE_MIN + tau

IF (THETA .LT. ANGLE_MIN) THEN
   REVS = AINT((ANGLE_MIN-THETA)/tau) + 1
   RHO = THETA + REVS*tau
ELSE IF (THETA .GE. ANGLE_MAX) THEN
   REVS = AINT((THETA-ANGLE_MIN)/tau)
   RHO = THETA - REVS*tau
ELSE
   RHO = THETA
END IF


END FUNCTION REDUCE





!***********************************************************************************************************************************
!  KEPLER
!
!  Solves the elliptical Kepler's equation by the Markley method.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION KEPLER (MA, ECC) RESULT (E5)

real(wp), INTENT(IN) :: MA
real(wp), INTENT(IN) :: ECC

!     Parameters.
!
!     PI           = PI
!     tau        = 2*PI
!     PI_SQR       = PI**2
!     ONEP6_PI     = 1.6*PI
!     TWO_THIRDS   = 2/3
!     SIXTH        = 1/6
!     R24          = 1/24
!

real(wp), PARAMETER :: PI = 4._wp * atan(1._wp)

!
!     Other variables.
!

real(wp) :: ALPHA, D, Q, R, W, E1
real(wp) :: F, F1, F2, F3, F4, DELTA3, DELTA4, DELTA5
real(wp) :: SE, M


!-----------------------------------------------------------------------------------------------------------------------------------

!
!     Start of code.
!
!     Put M in the range [-PI, PI) (required by the method).
!

M = REDUCE (MA, -PI)

!
!     Compute parameters.
!

ALPHA = (3*pi**2 + 1.6_wp*pi*(PI-ABS(M))/(1._wp+ECC)) / (pi**2 - 6.0D0)
D = 3.0D0*(1._wp-ECC) + ALPHA*ECC
Q = 2.0D0*ALPHA*D*(1._wp-ECC)-M**2
R = 3.0D0*ALPHA*D*(D-1.0D0+ECC)*M + M**3
W = (ABS(R) + SQRT(Q**3 + R**2)) ** (2._wp / 3._wp)

!
!     Compute first-order solution to Kepler's Equation (E1).
!

E1 = (2._wp*R*W/(W**2 + W*Q + Q**2) + M) / D

!
!     Save SIN(E1) into SE so we only have to evaluate it once.
!

SE = SIN(E1)

!
!     Find
!           F(E) = E - E SIN E - M
!
!     and its derivatives, through fourth order.
!

F  = E1 - ECC*SE - M
F1 = 1._wp - ECC*COS(E1)
F2 = ECC*SE
F3 = 1._wp - F1
F4 = -F2

!
!     Compute 3rd, 4th, and 5th-order corrections to E1.
!

DELTA3 = -F/(F1-0.5D0*F*F2/F1)
DELTA4 = -F/(F1+0.5D0*DELTA3*F2+ DELTA3**2*F3 / 6._wp)
DELTA5 = -F/(F1+0.5D0*DELTA4*F2+ DELTA4**2*F3 / 6._wp + DELTA4**3*F4 / 24._wp)

!
!     Find fifth-order refined estimate of E (E5).
!

E5 = E1 + DELTA5

!
!     Put E5 in the range [0, 2*PI) and return.
!

E5 = REDUCE (E5, 0._wp)

END FUNCTION KEPLER


elemental function toLower(str)
! Michael Hirsch
! can be trivially extended to non-ASCII
  character(*), intent(in) :: str
  character(len(str)) :: toLower
  character(*), parameter :: lower="abcdefghijklmnopqrstuvwxyz", &
                             upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  integer :: i,j

  toLower = str

  !do concurrent (i = 1:len(str)) ! FIXME: Flang
  do i=1,len(str)
    j = index(upper,str(i:i))
    if (j > 0) toLower(i:i) = lower(j:j)
  end do

end function toLower


elemental function toUpper(str)
! Michael Hirsch
! can be trivially extended to non-ASCII
  character(*), intent(in) :: str
  character(len(str)) :: toUpper
  character(*), parameter :: lower="abcdefghijklmnopqrstuvwxyz", &
                             upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  integer :: i,j

  toUpper = str

  !do concurrent (i = 1:len(str))  ! FIXME: Flang
  do i = 1,len(str)
    j = index(lower,str(i:i))
    if (j > 0) toUpper(i:i) = upper(j:j)
  end do

end function toUpper


end module funcs
