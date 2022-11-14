module fgamma

use assert, only: wp

implicit none

contains



!***********************************************************************************************************************************
!  PSI
!
!  Returns the digamma function of the argument XX.  XX cannot be 0 or a negative integer.
!
!  From https://netlib.org/specfun/psi
!***********************************************************************************************************************************

elemental real(wp) FUNCTION PSI(XX)
!----------------------------------------------------------------------
!
! This function program evaluates the logarithmic derivative of the
!   gamma function,
!
!      psi(x) = d/dx (gamma(x)) / gamma(x) = d/dx (ln gamma(x))
!
!   for real x, where either
!
!          -xmax1 < x < -xmin (x not a negative integer), or
!            xmin < x.
!
!   The calling sequence for this function is
!
!                  Y = PSI(X)
!
!   The main computation uses rational Chebyshev approximations
!   published in Math. Comp. 27, 123-127 (1973) by Cody, Strecok and
!   Thacher.  This transportable program is patterned after the
!   machine-dependent FUNPACK program PSI(X), but cannot match that
!   version for efficiency or accuracy.  This version uses rational
!   approximations that are theoretically accurate to 20 significant
!   decimal digits.  The accuracy achieved depends on the arithmetic
!   system, the compiler, the intrinsic functions, and proper selection
!   of the machine-dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   XINF   = largest positive machine number
!   XMAX1  = beta ** (p-1), where beta is the radix for the
!            floating-point system, and p is the number of base-beta
!            digits in the floating-point significand.  This is an
!            upper bound on non-integral floating-point numbers, and
!            the negative of the lower bound on acceptable negative
!            arguments for PSI.  If rounding is necessary, round this
!            value down.
!   XMIN1  = the smallest in magnitude acceptable argument.  We
!            recommend XMIN1 = MAX(1/XINF,xmin) rounded up, where
!            xmin is the smallest positive floating-point number.
!   XSMALL = absolute argument below which  PI*COTAN(PI*X)  may be
!            represented by 1/X.  We recommend XSMALL < sqrt(3 eps)/pi,
!            where eps is the smallest positive number such that
!            1+eps > 1.
!   XLARGE = argument beyond which PSI(X) may be represented by
!            LOG(X).  The solution to the equation
!               x*ln(x) = beta ** p
!            is a safe value.
!
!     Approximate values for some important machines are
!
!                        beta  p     eps     xmin       XINF
!
!  CDC 7600      (S.P.)    2  48  7.11E-15  3.13E-294  1.26E+322
!  CRAY-1        (S.P.)    2  48  7.11E-15  4.58E-2467 5.45E+2465
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)    2  24  1.19E-07  1.18E-38   3.40E+38
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)    2  53  1.11D-16  2.23E-308  1.79D+308
!  IBM 3033      (D.P.)   16  14  1.11D-16  5.40D-79   7.23D+75
!  SUN 3/160     (D.P.)    2  53  1.11D-16  2.23D-308  1.79D+308
!  VAX 11/780    (S.P.)    2  24  5.96E-08  2.94E-39   1.70E+38
!                (D.P.)    2  56  1.39D-17  2.94D-39   1.70D+38
!   (G Format)   (D.P.)    2  53  1.11D-16  5.57D-309  8.98D+307
!
!                         XMIN1      XMAX1     XSMALL    XLARGE
!
!  CDC 7600      (S.P.)  3.13E-294  1.40E+14  4.64E-08  9.42E+12
!  CRAY-1        (S.P.)  1.84E-2466 1.40E+14  4.64E-08  9.42E+12
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)  1.18E-38   8.38E+06  1.90E-04  1.20E+06
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)  2.23D-308  4.50D+15  5.80D-09  2.71D+14
!  IBM 3033      (D.P.)  1.39D-76   4.50D+15  5.80D-09  2.05D+15
!  SUN 3/160     (D.P.)  2.23D-308  4.50D+15  5.80D-09  2.71D+14
!  VAX 11/780    (S.P.)  5.89E-39   8.38E+06  1.35E-04  1.20E+06
!                (D.P.)  5.89D-39   3.60D+16  2.05D-09  2.05D+15
!   (G Format)   (D.P.)  1.12D-308  4.50D+15  5.80D-09  2.71D+14
!
!*******************************************************************
!*******************************************************************
!
! Error Returns
!
!  The program returns XINF for  X < -XMAX1, for X zero or a negative
!    integer, or when X lies in (-XMIN1, 0), and returns -XINF
!    when X lies in (0, XMIN1).
!
! Intrinsic functions required are:
!
!     ABS, AINT, DBLE, INT, LOG, REAL, TAN
!
!
!  Author: W. J. Cody
!          Mathematics and Computer Science Division
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!  Latest modification: June 8, 1988
!
!----------------------------------------------------------------------

real(wp), intent(in) :: xx

INTEGER I,N,NQ

real(wp) :: AUG,DEN,SGN,UPPER,W,X,Z

!----------------------------------------------------------------------
!  Mathematical constants.  PIOV4 = pi / 4
!----------------------------------------------------------------------
real(wp), parameter :: PIOV4 = 7.8539816339744830962D-01
!----------------------------------------------------------------------
!  Machine-dependent constants
!----------------------------------------------------------------------
!S    DATA XINF/1.70E+38/, XMIN1/5.89E-39/, XMAX1/8.38E+06/,
!S   1     XSMALL/1.35E-04/, XLARGE/1.20E+06/
real(wp), parameter :: XINF=1.70e+38_wp, XMIN1=5.89e-39_wp, XMAX1=3.60e+16+wp, &
XSMALL=2.05e-09_wp, XLARGE=2.04e+15_wp
!----------------------------------------------------------------------
!  Zero of psi(x)
!----------------------------------------------------------------------
real(wp), parameter :: X01=187._wp, X01D=128._wp, X02=6.9464496836234126266e-04_wp
!----------------------------------------------------------------------
!  Coefficients for approximation to  psi(x)/(x-x0)  over [0.5, 3.0]
!----------------------------------------------------------------------
real(wp), parameter :: P1(9)=[4.5104681245762934160D-03,5.4932855833000385356D+00,      &
            3.7646693175929276856D+02,7.9525490849151998065D+03,      &
            7.1451595818951933210D+04,3.0655976301987365674D+05,      &
            6.3606997788964458797D+05,5.8041312783537569993D+05,      &
            1.6585695029761022321D+05]
real(wp), parameter :: Q1(8)=[9.6141654774222358525D+01,2.6287715790581193330D+03,      &
            2.9862497022250277920D+04,1.6206566091533671639D+05,      &
            4.3487880712768329037D+05,5.4256384537269993733D+05,      &
            2.4242185002017985252D+05,6.4155223783576225996D-08]
!----------------------------------------------------------------------
!  Coefficients for approximation to  psi(x) - ln(x) + 1/(2x)
!     for  x > 3.0
!----------------------------------------------------------------------
real(wp), parameter :: P2(7)=[-2.7103228277757834192D+00,-1.5166271776896121383D+01,    &
              -1.9784554148719218667D+01,-8.8100958828312219821D+00,    &
              -1.4479614616899842986D+00,-7.3689600332394549911D-02,    &
              -6.5135387732718171306D-21]
real(wp), parameter :: Q2(6)=[4.4992760373789365846D+01, 2.0240955312679931159D+02,    &
               2.4736979003315290057D+02, 1.0742543875702278326D+02,    &
               1.7463965060678569906D+01, 8.8427520398873480342D-01]
!----------------------------------------------------------------------
!S    CONV(I) = REAL(I)

X = XX
W = ABS(X)
AUG = 0
!----------------------------------------------------------------------
!  Check for valid arguments, then branch to appropriate algorithm
!----------------------------------------------------------------------
IF ((-X .GE. XMAX1) .OR. (W .LT. XMIN1)) THEN
      GO TO 410
      ELSE IF (X >= 0.5_wp) THEN
      GO TO 200
!----------------------------------------------------------------------
!  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x)
!     Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL.
!----------------------------------------------------------------------
      ELSE IF (W .LE. XSMALL) THEN
      AUG = -1 / X
      GO TO 150
END IF
!----------------------------------------------------------------------
!  Argument reduction for cot
!----------------------------------------------------------------------
  100 IF (X .LT. 0) THEN
            SGN = PIOV4
         ELSE
            SGN = -PIOV4
      END IF
      W = W - AINT(W)
      NQ = INT(W * 4)
      W = 4 * (W - real(NQ, wp) * 0.25_wp)
!----------------------------------------------------------------------
!  W is now related to the fractional part of  4.0 * X.
!     Adjust argument to correspond to values in the first
!     quadrant and determine the sign.
!----------------------------------------------------------------------
      N = NQ / 2
      IF ((N+N) .NE. NQ) W = 1 - W
      Z = PIOV4 * W
      IF (MOD(N,2) .NE. 0) SGN = - SGN
!----------------------------------------------------------------------
!  determine the final value for  -pi * cotan(pi*x)
!----------------------------------------------------------------------
      N = (NQ + 1) / 2
      IF (MOD(N,2) .EQ. 0) THEN
!----------------------------------------------------------------------
!  Check for singularity
!----------------------------------------------------------------------
            IF (Z .EQ. 0) GO TO 410
            AUG = SGN * (4 / TAN(Z))
         ELSE
            AUG = SGN * (4 * TAN(Z))
      END IF
  150 X = 1 - X
  200 IF (X > 3) GO TO 300
!----------------------------------------------------------------------
!  0.5 <= X <= 3.0
!----------------------------------------------------------------------
      DEN = X
      UPPER = P1(1) * X
      DO I = 1, 7
         DEN = (DEN + Q1(I)) * X
         UPPER = (UPPER + P1(I+1)) * X
      end do
      DEN = (UPPER + P1(9)) / (DEN + Q1(8))
      X = (X-X01/X01D) - X02
      PSI = DEN * X + AUG
      GO TO 500
!----------------------------------------------------------------------
!  3.0 < X
!----------------------------------------------------------------------
  300 IF (X .LT. XLARGE) THEN
         W = 1 / (X * X)
         DEN = W
         UPPER = P2(1) * W
         DO 310 I = 1, 5
            DEN = (DEN + Q2(I)) * W
            UPPER = (UPPER + P2(I+1)) * W
  310    CONTINUE
         AUG = (UPPER + P2(7)) / (DEN + Q2(6)) - 0.5_wp / X + AUG
      END IF
      PSI = AUG + LOG(X)
      GO TO 500
!----------------------------------------------------------------------
!  Error return
!----------------------------------------------------------------------
  410 PSI = XINF
      IF (X .GT. 0) PSI = -XINF
  500 RETURN
!---------- Last card of PSI ----------
      END function psi


!***********************************************************************************************************************************
!  CGAMMA
!
!  Complex gamma function.
!  Formulae from "An Atlas of Functions" by Spanier and Oldham, Sect. 43:11.
!***********************************************************************************************************************************

      elemental complex(wp) FUNCTION CGAMMA (Z) RESULT (R)
      use hyper, only: csch

      COMPLEX(wp), INTENT(IN) :: Z

      real(wp), PARAMETER :: EPS = 1.0D-14
      real(wp), PARAMETER :: PI = 4 * atan(1._wp)
      real(wp), PARAMETER :: EULER = 0.57721566490153286060651209008240243104215933593992359880576723488486772677766467094_wp

      INTEGER :: J
      real(wp) :: X, Y, THETA, SUM, PROD, PSUM, PPROD


      X = Z%RE
      Y = Z%IM

      IF (Z .EQ. (0.0D0,0.0D0)) THEN
         ! write(stderr, *) '  CGAMMA Error.'
         R = (0.0D0,0.0D0)
         RETURN
      END IF

      IF (Y .EQ. 0.0D0) THEN                                                        ! if real Z
         R = CMPLX(GAMMA(X),0.0D0, wp)
         RETURN
      END IF

      IF (X .EQ. 0.0D0) GO TO 100                                                   ! branch for imaginary Z

!
!     Complex Z
!

      SUM = 0.0D0
      PSUM = HUGE(0._wp)

      J = 0
      DO
         SUM = SUM + Y/(real(j,wp)+X) - ATAN2(Y, real(J,wp)+X)
         IF (ABS((SUM-PSUM)/SUM) .LE. EPS) EXIT
         PSUM = SUM
         J = J + 1
         IF (J .LT. 0) EXIT
      END DO

      THETA = Y*PSI(X) + SUM

      PROD = 1
      PPROD = HUGE(0._wp)

      J = 0
      DO
         PROD = PROD * ABS(real(j,wp)+X)/SQRT(Y**2+(real(J,wp)+X)**2)
         IF (ABS((PROD-PPROD)/PROD) .LE. EPS) EXIT
         PPROD = PROD
         J = J + 1
         IF (J .LT. 0) EXIT
      END DO

      R = CMPLX(COS(THETA),SIN(THETA), wp) * ABS(gamma(X)) * PROD

      RETURN

!
!     Imaginary Z
!

  100 SUM = 0.0D0
      PSUM = HUGE(0._wp)

      J = 1
      DO
         SUM = SUM + Y/real(j,wp) - ATAN2(Y,real(j,wp))
         IF (ABS((SUM-PSUM)/SUM) .LE. EPS) EXIT
         PSUM = SUM
         J = J + 1
         IF (J .LT. 0) EXIT
      END DO

      THETA = -EULER*Y + SUM

      R = SQRT((PI/Y)*CSCH(PI*Y)) * CMPLX(SIN(THETA),-COS(THETA), wp)

      END FUNCTION CGAMMA





!***********************************************************************************************************************************
!  BETA
!
!  Beta function.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION BETA (X,Y) RESULT (R)

real(wp), INTENT(IN) :: X, Y

R = gamma(X)*gamma(Y)/gamma(X+Y)

END FUNCTION BETA

!***********************************************************************************************************************************
!  CBETA
!
!  Complex beta function.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CBETA (X,Y) RESULT (R)

COMPLEX(wp), INTENT(IN) :: X, Y


R = CGAMMA(X)*CGAMMA(Y)/CGAMMA(X+Y)

END FUNCTION CBETA

!***********************************************************************************************************************************
!  RBETA
!
!  Rational beta function.
!***********************************************************************************************************************************

elemental SUBROUTINE RBETA (X, Y, N, D)
use rat, only: ratnorm

INTEGER, INTENT(IN) :: X, Y
INTEGER, INTENT(OUT) :: N, D
INTEGER :: I


N = 1

DO I = 2, X-1
   N = N * I
END DO

DO I = 2, Y-1
   N = N * I
END DO

D = 1

DO I = 2, X+Y-1
   D = D * I
END DO

CALL RATNORM (N, D)

END SUBROUTINE RBETA

end module fgamma
