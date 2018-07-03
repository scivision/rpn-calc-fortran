module evals

use global
use funcs
use stackops
use assert, only: wp, isclose

implicit none
private

complex(wp), parameter, private :: C0 = (0._wp, 0._wp)


public :: eval

contains


!***********************************************************************************************************************************
!  EVAL
!
!  Evaluate a operation.
!***********************************************************************************************************************************

SUBROUTINE EVAL (STR)

CHARACTER(*), INTENT(IN) :: STR

real(wp), PARAMETER :: PI = 4._wp * atan(1._wp)
real(wp), PARAMETER :: EULER = 0.5772156649_wp
real(wp), PARAMETER :: GOLDEN = 1.618033988745_wp
COMPLEX(wp),  PARAMETER :: II = (0._wp,1._wp)
real(wp), PARAMETER :: KG_PER_LB = 0.45359237D0
real(wp), PARAMETER :: CM_PER_IN = 2.54_wp
real(wp), PARAMETER :: L_PER_GAL = 3.785411784D0
real(wp), PARAMETER :: A0 = 0.5291772108D-10                          ! m
real(wp), PARAMETER :: AMU = 1.660539040e-27_wp                       ! kg
real(wp), PARAMETER :: AU = 1.49597870D11                             ! m
real(wp), PARAMETER :: C = 299792458.0D0                              ! m/s
real(wp), PARAMETER :: ECHG = 1.6021766e-19_wp                        ! C
real(wp), PARAMETER :: EPS0 = 8.8541878176203898505D-12               ! F/m
real(wp), PARAMETER :: G = 9.80665D0                                  ! m/s^2
real(wp), PARAMETER :: GRAV = 6.6742D-11                              ! m^3/kg s^2
real(wp), PARAMETER :: H = 6.6260693D-34                              ! J s
real(wp), PARAMETER :: HBAR = 1.05457168D-34                          ! J s
real(wp), PARAMETER :: KB = 1.3806505D-23                             ! J/K
real(wp), PARAMETER :: ME = 9.1093826D-31                             ! kg
real(wp), PARAMETER :: MN = 1.67492728D-27                            ! kg
real(wp), PARAMETER :: MP = 1.67262171D-27                            ! kg
real(wp), PARAMETER :: MU0 = 12.5663706143591729539D-7                ! N/A^2
real(wp), PARAMETER :: MUB = 927.400949D-26                           ! A m^2
real(wp), PARAMETER :: MUN = 5.05078343D-27                           ! A m^2
real(wp), PARAMETER :: NA = 6.0221415D23                              ! mol^-1
real(wp), PARAMETER :: REARTH = 6378140.0D0                           ! m
real(wp), PARAMETER :: RGAS = 8.314472D0                              ! J/mol K
real(wp), PARAMETER :: STEFAN = 5.670400D-8                           ! W/m^2 K^4

INTEGER :: I, ITMP, ITMP2, NUM, DEN, NUM2, DEN2, NUM3, DEN3, NUM4, DEN4, &
   NUMM, DENM, NUMB, DENB
real(wp) :: TMP, TMP2, TMP3, TMPM, TMPB, TMPR, BES_X, BES_ALPHA
COMPLEX(wp) :: CTMP, CTMPM, CTMPB, CTMPR
CHARACTER(LEN=2) :: REGNAME
INTEGER :: DT(8)
INTEGER :: YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, BES_NB, BES_NCALC
CHARACTER(LEN=100) :: NUMSTR
CHARACTER(LEN=10) :: TIME, DATE, ZONE

real(wp), ALLOCATABLE, DIMENSION(:) :: BES_B

IF (LEN_TRIM(STR) == 0) return

select case(str)

case('+')                                                  ! +
  call add(domain_mode)
  
case('-')                                                  ! -
  call subtract(domain_mode)
  
case('*')                                                  ! *
  call multiply(domain_mode)

case('/')                                                  ! /
  call divide(domain_mode)

case('^')                                                  ! ^
  call power(domain_mode)

case('\')                                                  ! \
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(stack(1), 0._wp)) THEN
            write(stderr, *) '  Divide Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = 1._wp / STACK(1)
         END IF
      CASE (2)
         IF (isclose(CSTACK(1), C0)) THEN
            write(stderr, *) '  Divide Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = (1.0,0.0) / CSTACK(1)
         END IF
      CASE (3)
         CALL RDIV (1,1,RNSTACK(1),RDSTACK(1),NUM,DEN)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM
         RDSTACK(1) = DEN
   END SELECT

case('%')                                                  ! %
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(2) * 0.01_wp*STACK(1)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(2) * 0.01_wp*CSTACK(1)
      CASE (3)
         NUM = RNSTACK(1)
         DEN = RDSTACK(1)
         NUM2 = RNSTACK(2)
         DEN2 = RDSTACK(2)
         CALL RMUL(NUM2,DEN2,NUM,DEN,ITMP,ITMP2)
         CALL RMUL(ITMP,ITMP2,1,100,NUM,DEN)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM
         RDSTACK(1) = DEN
   END SELECT

case('%CHG')                                               ! %CHG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(STACK(2), 0._wp)) THEN
            write(stderr, *) '  Divide Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = 100._wp*(STACK(1)-STACK(2))/STACK(2)
         END IF
      CASE (2)
         IF (isclose(CSTACK(2), c0)) THEN
            write(stderr, *) '  Divide Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = 100._wp*(CSTACK(1)-CSTACK(2))/CSTACK(2)
         END IF
      CASE (3)
         NUM = RNSTACK(1)
         DEN = RDSTACK(1)
         NUM2 = RNSTACK(2)
         DEN2 = RDSTACK(2)
         CALL RSUB(NUM,DEN,NUM2,DEN2,ITMP,ITMP2)
         CALL RDIV(ITMP,ITMP2,NUM2,DEN2,NUM,DEN)
         CALL RMUL(NUM,DEN,100,1,NUM2,DEN2)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM2
         RDSTACK(1) = DEN2
   END SELECT

case('!')                                                  ! !
  SELECT CASE (DOMAIN_MODE)
    CASE (1)
      IF (ISINT(STACK(1)).AND.(STACK(1) < 0._wp)) THEN
        write(stderr, *) '  Factorial Error'
        return
      endif

      LASTX = STACK(1)
      STACK(1) = gamma(STACK(1)+1._wp)

    CASE (2)
      IF (isclose(CSTACK(1), (-1._wp, 0._wp))) THEN
        write(stderr, *) '  Factorial Error'
        return
      endif
     
      CLASTX = CSTACK(1)
      CSTACK(1) = CGAMMA(CSTACK(1)+(1._wp, 0._wp))
     
    CASE (3)
      IF ((RDSTACK(1)==1).AND.(RNSTACK(1)<0)) THEN
        write(stderr, *) '  Factorial Error'
        return
      endif
      
      IF (RDSTACK(1)==1) THEN
         ITMP = RNSTACK(1)
         IF (ITMP<0) THEN
            write(stderr, *) '  Factorial Error'
            return
         endif   
         
         ITMP2 = 1
         DO I = 2, ITMP
           ITMP2 = ITMP2 * I
         END DO
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = ITMP2
         RDSTACK(1) = 1
      ELSE
         CALL SWITCH_RAT_TO_REAL
         IF (ISINT(STACK(1)).AND.(STACK(1)<0.0D0)) THEN
           write(stderr, *) '  Factorial Error'
           return
         endif
         LASTX = STACK(1)
         STACK(1) = gamma(STACK(1)+1._wp)
      END IF
   END SELECT

case('!!')                                                 ! !!
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) < 0._wp) THEN
            write(stderr, *) '  !! Error'
         ELSE IF (ISFRAC(STACK(1))) THEN
            write(stderr, *) '  !! Error'
         ELSE IF (NINT(STACK(1)) == 0) THEN
            LASTX = STACK(1)
            STACK(1) = 1._wp
         ELSE
            LASTX = STACK(1)
            ITMP = NINT(STACK(1))
            STACK(1) = 1._wp
            DO
               STACK(1) = STACK(1) * ITMP
               ITMP = ITMP - 2
               IF (ITMP <= 1) EXIT
            END DO
         END IF
      CASE (2)
         IF (real(CSTACK(1), wp) < 0._wp) THEN
            write(stderr, *) '  !! Error'
         ELSE IF (.not.isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            write(stderr, *) '  !! Error'
         ELSE IF (ISFRAC(real(CSTACK(1), wp))) THEN
            write(stderr, *) '  !! Error'
         ELSE IF (NINT(real(CSTACK(1), wp)) == 0) THEN
            CLASTX = CSTACK(1)
            CSTACK(1) = (1._wp, 0._wp)
         ELSE
            CLASTX = CSTACK(1)
            ITMP = NINT(real(CSTACK(1), wp))
            TMP = 1._wp
            DO
               TMP = TMP * ITMP
               ITMP = ITMP - 2
               IF (ITMP <= 1) EXIT
            END DO
            CSTACK(1) = CMPLX(TMP, 0._wp, wp)
         END IF
      CASE (3)
         IF (RNSTACK(1) < 0) THEN
            write(stderr, *) '  !! Error'
         ELSE IF (RDSTACK(1) /= 1) THEN
            write(stderr, *) '  !! Error'
         ELSE IF (RNSTACK(1) == 0) THEN
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = 1
            RDSTACK(1) = 1
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            ITMP = RNSTACK(1)
            RNSTACK(1) = 1
            RDSTACK(1) = 1
            DO
               RNSTACK(1) = RNSTACK(1) * ITMP
               ITMP = ITMP - 2
               IF (ITMP <= 1) EXIT
            END DO
         END IF
   END SELECT

case('10X')                                                ! 10X
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = 10._wp**(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = 10._wp**(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = 10._wp**(STACK(1))
   END SELECT

case('2PI')                                                ! 2PI
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK(2*pi)
      CASE (2)
         CALL PUSH_STACK(CMPLX(2*pi, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK(2*pi)
   END SELECT

case('2PII')                                               ! 2PII
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         write(stderr, *) ' 2PIi not available in REAL mode'
      CASE (2)
         CALL PUSH_STACK(CMPLX(0._wp,2*pi, wp))
      CASE (3)
         write(stderr, *) ' 2PIi not available in RATIONAL mode'
   END SELECT

case('2X')                                                 ! 2X
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = 2.0D0**(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = 2.0D0**(STACK(1))
   END SELECT

case('A0')                                                 ! A0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
      CASE (2)
         CALL push_stack(CMPLX(A0, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (A0)
   END SELECT

case('ABS')                                                ! ABS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = ABS(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(ABS(CSTACK(1)), 0._wp, wp)
      CASE (3)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = ABS(RNSTACK(1))
         RDSTACK(1) = ABS(RDSTACK(1))
   END SELECT

case('ACOS')                                               ! ACOS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) > 1._wp) THEN
            write(stderr, *) '  ACOS Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOS(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = acos(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) > ABS(RDSTACK(1))) THEN
            write(stderr, *) '  ACOS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOS(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('ACOSH')                                             ! ACOSH
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

case('ACOT')                                               ! ACOT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = ACOT(STACK(1))/ANGLE_FACTOR
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ACOT(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = ACOT(STACK(1))/ANGLE_FACTOR
   END SELECT

case('ACOT2')                                             ! ACOT2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = ACOT2(STACK(2),STACK(1))/ANGLE_FACTOR
         CALL DROP_STACK(2)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ACOT(CSTACK(2)/CSTACK(1))/ANGLE_FACTOR
         CALL CDROP_STACK(2)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = ACOT2(STACK(2),STACK(1))/ANGLE_FACTOR
         CALL DROP_STACK(2)
   END SELECT

case('ACOTH')                                             ! ACOTH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(stack(1), 0._wp)) THEN
            write(stderr, *) '  ACOTH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOTH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACOTH(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) == 0) THEN
            write(stderr, *) '  ACOTH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOTH(STACK(1))
         END IF
   END SELECT

case('ACOVERS')                                            ! ACOVERS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(1._wp-STACK(1)) > 1._wp) THEN
            write(stderr, *) '  ACOVERS Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOVERS(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACOVERS(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) < 0) THEN
            write(stderr, *) '  ACOVERS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOVERS(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('ACRD')                                               ! ACRD
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) > 2.0D0) THEN
            write(stderr, *) '  ACRD Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACRD(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACRD(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) > 2*ABS(RDSTACK(1))) THEN
            write(stderr, *) '  ACRD Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACRD(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('ACSC')                                               ! ACSC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) < 1._wp) THEN
            write(stderr, *) '  ACSC Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACSC(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ACSC(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) < ABS(RDSTACK(1))) THEN
            write(stderr, *) '  ACSC Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACSC(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('ACSCH')                                             ! ACSCH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(stack(1), 0._wp)) THEN
            write(stderr, *) '  ACSCH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACSCH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACSCH(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) == 0) THEN
            write(stderr, *) '  ACSCH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACSCH(STACK(1))
         END IF
   END SELECT

case('AEXSEC')                                             ! AEXSEC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)+1._wp) < 1._wp) THEN
            write(stderr, *) '  AEXSEC Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AEXSEC(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CAEXSEC(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) < 0) THEN
            write(stderr, *) '  AEXSEC Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = AEXSEC(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('AHAV')                                               ! AHAV
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1)<0.0D0).OR.(STACK(1)>1._wp)) THEN
            write(stderr, *) '  AHAV Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AHAV(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = AHAV(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF ((RNSTACK(1)<0).OR.(RNSTACK(1)>RDSTACK(1))) THEN
            write(stderr, *) '  AHAV Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = AHAV(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('ALL')                                                 ! ALL
   DISP_MODE = 4

case('AMU')                                                ! AMU
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (AMU)
      CASE (2)
         CALL push_stack(CMPLX(AMU, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (AMU)
   END SELECT

case('AND')                                                ! AND
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = IAND (INT(STACK(2)), INT(STACK(1)))
         CALL DROP_STACK(2)
      CASE (2)
         TMP = IAND (INT(real(CSTACK(2), wp)), INT(real(CSTACK(1), wp)))
         TMP2 = IAND (INT(AIMAG(CSTACK(2))), INT(AIMAG(CSTACK(1))))
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
         CALL CDROP_STACK(2)
      CASE (3)
         ITMP = RNSTACK(1)/RDSTACK(1)
         ITMP2 = RNSTACK(2)/RDSTACK(2)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = IAND (ITMP2, ITMP)
         RDSTACK(1) = 1
         CALL RDROP_STACK(2)
   END SELECT

case('ARG')                                                ! ARG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = 0._wp
      CASE (2)
         TMP = ATAN2(AIMAG(CSTACK(1)),real(CSTACK(1), wp))/ANGLE_FACTOR
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP, 0._wp, wp)
      CASE (3)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = 0
         RDSTACK(1) = 1
   END SELECT

case('ASEC')                                               ! ASEC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) < 1._wp) THEN
            write(stderr, *) '  ASEC Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ASEC(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ASEC(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) < ABS(RDSTACK(1))) THEN
            write(stderr, *) '  ASEC Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            STACK(1) = ASEC(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('ASECH')                                             ! ASECH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1)<=0.0D0).OR.(STACK(1)>1._wp)) THEN
            write(stderr, *) '  ASECH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ASECH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CASECH(CSTACK(1))
      CASE (3)
         IF ((RNSTACK(1)<=0).OR.(RNSTACK(1)>RDSTACK(1))) THEN
            write(stderr, *) '  ASECH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ASECH(STACK(1))
         END IF
   END SELECT

case('ASIN')                                               ! ASIN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) > 1._wp) THEN
            write(stderr, *) '  ASIN Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ASIN(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ASIN(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) > ABS(RDSTACK(1))) THEN
            write(stderr, *) '  ASIN Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ASIN(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('ASINH')                                             ! ASINH
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

case('ATAN')                                               ! ATAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = ATAN(STACK(1))/ANGLE_FACTOR
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ATAN(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = ATAN(STACK(1))/ANGLE_FACTOR
   END SELECT

case('ATAN2')                                             ! ATAN2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = ATAN2(STACK(2),STACK(1))/ANGLE_FACTOR
         CALL DROP_STACK(2)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ATAN(CSTACK(2)/CSTACK(1))/ANGLE_FACTOR
         CALL CDROP_STACK(2)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = ATAN2(STACK(2),STACK(1))/ANGLE_FACTOR
         CALL DROP_STACK(2)
   END SELECT

case('ATANH')                                             ! ATANH
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

case('AU')                                                 ! AU
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (AU)
      CASE (2)
         CALL push_stack(CMPLX(AU, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (AU)
   END SELECT

case('AVERS')                                             ! AVERS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(1._wp-STACK(1)) > 1._wp) THEN
            write(stderr, *) '  AVERS Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AVERS(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CAVERS(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) < 0) THEN
            write(stderr, *) '  AVERS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = AVERS(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

case('BESSELJ0')                                           ! BESSELJ0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = bessel_J0(STACK(1))
      CASE (2)
         write(stderr, *) '  Error:  BESSELJ0 not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = bessel_j0(STACK(1))
   END SELECT

case('BESSELJ1')                                           ! BESSELJ1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = bessel_j1(STACK(1))
      CASE (2)
         write(stderr, *) '  Error:  BESSELJ1 not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = bessel_j1(STACK(1))
   END SELECT

case('BESSELJ')                                            ! BESSELJ
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELJ Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RJBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC < 0) THEN
               write(stderr, *) '  BESSELJ Error 2'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELJ Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELJ not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELJ Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RJBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC < 0) THEN
               write(stderr, *) '  BESSELJ Error 2'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELJ Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

case('BESSELY0')                                           ! BESSELY0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELY0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y0(STACK(1))
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELY0 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELY0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y0(STACK(1))
         END IF
   END SELECT

case('BESSELY1')                                           ! BESSELY1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELY1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y1(STACK(1))
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELY1 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELY1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y1(STACK(1))
         END IF
   END SELECT

case('BESSELY')                                            ! BESSELY
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELY Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RYBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC < 0) THEN
               write(stderr, *) '  BESSELY Error 2'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELY Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELY not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELY Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RYBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC < 0) THEN
               write(stderr, *) '  BESSELY Error 2'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELY Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

case('BESSELI0')                                           ! BESSELI0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = BESI0(STACK(1))
      CASE (2)
         write(stderr, *) '  Error:  BESSELI0 not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = BESI0(STACK(1))
   END SELECT

case('BESSELI1')                                           ! BESSELI1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = BESI1(STACK(1))
      CASE (2)
         write(stderr, *) '  Error:  BESSELI1 not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = BESI1(STACK(1))
   END SELECT

case('BESSELI')                                            ! BESSELI
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELI Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RIBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC < 0) THEN
               write(stderr, *) '  BESSELI Error 2'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELI Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELI not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELI Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RIBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC < 0) THEN
               write(stderr, *) '  BESSELI Error 2'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELI Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

case('BESSELK0')                                           ! BESSELK0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELK0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK0(STACK(1))
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELK0 not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELK0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK0(STACK(1))
         END IF
   END SELECT

case('BESSELK1')                                           ! BESSELK1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELK1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK1(STACK(1))
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELK1 not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  BESSELK1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK1(STACK(1))
         END IF
   END SELECT

case('BESSELK')                                            ! BESSELK
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELK Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RKBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC < -1) THEN
               write(stderr, *) '  BESSELK Error 2'
            ELSE IF (BES_NCALC == -1) THEN
               write(stderr, *) '  BESSELK Error 3'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELK Error 4'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         write(stderr, *) '  Error:  BESSELK not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  BESSELK Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RKBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC < -1) THEN
               write(stderr, *) '  BESSELK Error 2'
            ELSE IF (BES_NCALC == -1) THEN
               write(stderr, *) '  BESSELK Error 3'
            ELSE IF (BES_NCALC /= BES_NB) THEN
               write(stderr, *) '  BESSELK Error 4'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

case('BETA')                                               ! BETA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((ISINT(STACK(1)).AND.(STACK(1)<=0.0D0)) .OR.  &
             (ISINT(STACK(2)).AND.(STACK(2)<=0.0D0))) THEN
            write(stderr, *) '  BETA Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BETA(STACK(1),STACK(2))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CBETA(CSTACK(1),CSTACK(2))
         CALL CDROP_STACK(2)
      CASE (3)
         IF (((RDSTACK(1)==1).AND.(RNSTACK(1)<=0)) .OR. &
             ((RDSTACK(2)==1).AND.(RNSTACK(2)<=0))) THEN
            write(stderr, *) '  BETA Error'
         ELSE
            IF ((RDSTACK(1)==1) .AND. (RDSTACK(2)==1)) THEN
               ITMP = RNSTACK(1)
               IF (ITMP<=0) THEN
                  write(stderr, *) '  BETA Error'
               ELSE
                  CALL RBETA(RNSTACK(1),RNSTACK(2),ITMP,ITMP2)
                  RNLASTX = RNSTACK(1)
                  RDLASTX = RDSTACK(1)
                  RNSTACK(1) = ITMP
                  RDSTACK(1) = ITMP2
                  CALL RDROP_STACK(2)
               END IF
            ELSE
               CALL SWITCH_RAT_TO_REAL
               IF ((ISINT(STACK(1)).AND.(STACK(1)<=0.0D0)) .OR.  &
                   (ISINT(STACK(2)).AND.(STACK(2)<=0.0D0))) THEN
                  write(stderr, *) '  BETA Error'
               ELSE
                  LASTX = STACK(1)
                  STACK(1) = BETA(STACK(1),STACK(2))
                  CALL DROP_STACK(2)
               END IF
            END IF
         END IF
   END SELECT

case('BIN')                                                ! BIN
   BASE_MODE = 2

case('C')                                                  ! C
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (C)
      CASE (2)
         CALL push_stack(CMPLX(C, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (C)
   END SELECT

case('C>F')                                                ! C>F
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = (9.0D0/5.0D0)*STACK(1)+32.0D0
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = (9.0D0/5.0D0)*CSTACK(1)+32.0D0
      CASE (3)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         CALL RMUL (9,5,RNSTACK(1),RDSTACK(1),NUM,DEN)
         CALL RADD (NUM,DEN,32,1,NUM2,DEN2)
         RNSTACK(1) = NUM2
         RDSTACK(1) = DEN2
   END SELECT

case('CBRT')                                               ! CBRT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = CUBEROOT(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CUBEROOT(CSTACK(1))
      CASE (3)
         TMP = CUBEROOT(real(RNSTACK(1), wp))
         TMP2 = CUBEROOT(real(RDSTACK(1), wp))
         IF (ISFRAC(TMP).OR.ISFRAC(TMP2)) THEN
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = CUBEROOT(STACK(1))
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = NINT(CUBEROOT(real(RNSTACK(1), wp)))
            RDSTACK(1) = NINT(CUBEROOT(real(RDSTACK(1), wp)))
         END IF
   END SELECT

case('CHS')                                                ! CHS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK(1) = -STACK(1)
      CASE (2)
         CSTACK(1) = -CSTACK(1)
      CASE (3)
         RNSTACK(1) = -RNSTACK(1)
   END SELECT

case('CLALL')                                             ! CLALL
   SELECT CASE (DOMAIN_MODE)
      CASE(1)
         STACK = 0._wp
         REG = 0._wp
         NN = 0._wp
         SUMX = 0._wp
         SUMX2 = 0._wp
         SUMY = 0._wp
         SUMY2 = 0._wp
         SUMXY = 0._wp
      CASE (2)
         CSTACK = (0._wp, 0._wp)
         CREG = (0._wp, 0._wp)
         CNN = (0._wp, 0._wp)
         CSUMX = (0._wp, 0._wp)
         CSUMX2 = (0._wp, 0._wp)
         CSUMY = (0._wp, 0._wp)
         CSUMY2 = (0._wp, 0._wp)
         CSUMXY = (0._wp, 0._wp)
      CASE (3)
         RNSTACK = 0; RDSTACK = 1
         RNREG = 0; RDREG = 1
         RNNN = 0; RDNN = 1
         RNSUMX = 0; RDSUMX = 1
         RNSUMX2 = 0; RDSUMX2 = 1
         RNSUMY = 0; RDSUMY = 1
         RNSUMY2 = 0; RDSUMY2 = 1
         RNSUMXY = 0; RDSUMXY = 1
   END SELECT

case('CLREG')                                             ! CLREG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         REG = 0._wp
      CASE (2)
         CREG = (0._wp, 0._wp)
      CASE (3)
         RNREG = 0; RDREG = 1
   END SELECT

case('CLS')                                                ! CLS
   SELECT CASE (DOMAIN_MODE)
      CASE(1)
         NN = 0._wp
         SUMX = 0._wp
         SUMX2 = 0._wp
         SUMY = 0._wp
         SUMY2 = 0._wp
         SUMXY = 0._wp
      CASE (2)
         CNN = (0._wp, 0._wp)
         CSUMX = (0._wp, 0._wp)
         CSUMX2 = (0._wp, 0._wp)
         CSUMY = (0._wp, 0._wp)
         CSUMY2 = (0._wp, 0._wp)
         CSUMXY = (0._wp, 0._wp)
      CASE (3)
         RNNN = 0; RDNN = 1
         RNSUMX = 0; RDSUMX = 1
         RNSUMX2 = 0; RDSUMX2 = 1
         RNSUMY = 0; RDSUMY = 1
         RNSUMY2 = 0; RDSUMY2 = 1
         RNSUMXY = 0; RDSUMXY = 1
   END SELECT

case('CLSTK')                                             ! CLSTK
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK = 0._wp
      CASE (2)
         CSTACK = (0._wp, 0._wp)
      CASE (3)
         RNSTACK = 0; RDSTACK = 1
   END SELECT

case('CLX')                                                ! CLX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK(1) = 0._wp
      CASE (2)
         CSTACK(1) = (0._wp, 0._wp)
      CASE (3)
         RNSTACK(1) = 0; RDSTACK(1) = 1
   END SELECT

case('CM>IN')                                             ! CM>IN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK(1) = STACK(1) / CM_PER_IN
      CASE (2)
         CSTACK(1) = CSTACK(1) / CM_PER_IN
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         STACK(1) = STACK(1) / CM_PER_IN
   END SELECT

case('CNR')                                                ! CNR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISFRAC(STACK(1)) .OR. ISFRAC(STACK(2))) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF (STACK(2) < STACK(1)) THEN
            write(stderr, *) '  CNR Error'
         ELSE
            ITMP  = NINT(STACK(1))
            ITMP2 = NINT(STACK(2))
            LASTX = STACK(1)
            STACK(1) = CNR (ITMP2, ITMP)
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)) .OR. ISFRAC(real(CSTACK(2), wp))) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF (real(CSTACK(1), wp)<0.0D0) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF (real(CSTACK(2), wp)<0.0D0) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF (.not.isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF (.not.isclose(AIMAG(CSTACK(2)), 0._wp)) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF (real(CSTACK(2), wp) < real(CSTACK(1), wp)) THEN
            write(stderr, *) '  CNR Error'
         ELSE
            ITMP  = NINT(real(CSTACK(1), wp))
            ITMP2 = NINT(real(CSTACK(2), wp))
            TMP = CNR (ITMP2, ITMP)
            CLASTX = CSTACK(1)
            CSTACK(1) = CMPLX(TMP, 0._wp, wp)
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF ((RDSTACK(1)/=1).OR.(RDSTACK(2)/=1)) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF ((RNSTACK(1)<0) .OR. (RNSTACK(2)<0)) THEN
            write(stderr, *) '  CNR Error'
         ELSE IF (RNSTACK(2) < RNSTACK(1)) THEN
            write(stderr, *) '  CNR Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = int(CNR(RNSTACK(2), RNSTACK(1)))
            CALL RDROP_STACK(2)
         END IF
   END SELECT

case('COMPLEX')                                            ! COMPLEX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         DOMAIN_MODE = 2
         
         CSTACK = CMPLX(STACK, 0._wp, wp)
         
         CREG = CMPLX(REG, 0._wp, wp)
         
         CLASTX = CMPLX(LASTX, 0._wp, wp)
         CNN = CMPLX(NN, 0._wp, wp)
         CSUMX = CMPLX(SUMX, 0._wp, wp)
         CSUMX2 = CMPLX(SUMX2, 0._wp, wp)
         CSUMY = CMPLX(SUMY, 0._wp, wp)
         CSUMY2 = CMPLX(SUMY2, 0._wp, wp)
         CSUMXY = CMPLX(SUMXY, 0._wp, wp)
      CASE (3)
         DOMAIN_MODE = 2
         DO I = 1, STACK_SIZE
            TMP = real(RNSTACK(I), wp)/real(RDSTACK(I), wp)
            CSTACK(I) = CMPLX(TMP, 0._wp, wp)
         END DO
         DO I = 0, REG_SIZE-1
            TMP = DBLE(RNREG(I))/DBLE(RDREG(I))
            CREG(I) = CMPLX(TMP, 0._wp, wp)
         END DO
         CLASTX = CMPLX(real(RNLASTX, wp)/real(RDLASTX, wp), 0._wp, wp)
         CNN = CMPLX(real(RNNN, wp)/real(RDNN, wp), 0._wp, wp)
         CSUMX = CMPLX(real(RNSUMX, wp)/real(RDSUMX, wp), 0._wp, wp)
         CSUMX2 = CMPLX(real(RNSUMX2, wp)/real(RDSUMX2, wp), 0._wp, wp)
         CSUMY = CMPLX(real(RNSUMY, wp)/real(RDSUMY, wp), 0._wp, wp)
         CSUMY2 = CMPLX(real(RNSUMY2, wp)/real(RDSUMY2, wp), 0._wp, wp)
         CSUMXY = CMPLX(real(RNSUMXY, wp)/real(RDSUMXY, wp), 0._wp, wp)
   END SELECT

case('CONJ')                                               ! CONJ
   SELECT CASE (DOMAIN_MODE)
      CASE (2)
         CSTACK(1) = CONJG(CSTACK(1))
   END SELECT

case('COS')                                                ! COS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = COS(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = COS(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = COS(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('COSH')                                               ! COSH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = COSH(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = COSH(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = COSH(STACK(1))
   END SELECT

case('COT')                                                ! COT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = COT(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = COT(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = COT(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('COTH')                                               ! COTH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = COTH(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CCOTH(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = COTH(STACK(1))
   END SELECT

case('COVERS')                                             ! COVERS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = COVERS(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CCOVERS(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = COVERS(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('CRD')                                                ! CRD
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = CRD(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CRD(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = CRD(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('CSC')                                                ! CSC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = CSC(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSC(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = CSC(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('CSCH')                                               ! CSCH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = CSCH(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CCSCH(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = CSCH(STACK(1))
   END SELECT

case('CUBE')                                               ! CUBE
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1)**3
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1)**3
      CASE (3)
         NUM = RNSTACK(1)
         DEN = RDSTACK(1)
         CALL RMUL (NUM,DEN,NUM,DEN,NUM2,DEN2)
         CALL RMUL (NUM, DEN, NUM2, DEN2, NUM, DEN)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM
         RDSTACK(1) = DEN
   END SELECT

case('D>F')                                                ! D>F
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         CALL DEC_TO_FRAC (STACK(1), NUM, DEN, FRACTOL)
         CALL DROP_STACK(1)
         CALL PUSH_STACK(real(NUM, wp))
         CALL PUSH_STACK(real(DEN, wp))
      CASE (2)
         CLASTX = CSTACK(1)
         CALL DEC_TO_FRAC (real(CSTACK(1), wp), NUM, DEN, FRACTOL)
         CALL DEC_TO_FRAC (AIMAG(CSTACK(1)), NUM2, DEN2, FRACTOL)
         CALL CDROP_STACK(1)
         CALL push_stack(CMPLX(real(NUM, wp),real(NUM2, wp), wp))
         CALL push_stack(CMPLX(real(DEN, wp),real(DEN2, wp), wp))
   END SELECT

case('D>R')                                                ! D>R
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1)*PI/180.0D0
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1)*PI/180.0D0
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(1)*PI/180.0D0
   END SELECT

case('DEC')                                                ! DEC
   BASE_MODE = 10

case('DEFAULT')                                            ! DEFAULT
   ANGLE_MODE = INITIAL_ANGLE_MODE

   SELECT CASE (ANGLE_MODE)
      CASE (1)
         ANGLE_FACTOR = PI/180.0D0
      CASE (2)
         ANGLE_FACTOR = 1._wp
      CASE (3)
         ANGLE_FACTOR = PI/200.0D0
      CASE (4)
         ANGLE_FACTOR = 2*pi
   END SELECT

   DISP_MODE = INITIAL_DISP_MODE
   DISP_DIGITS = INITIAL_DISP_DIGITS
   DOMAIN_MODE = INITIAL_DOMAIN_MODE
   BASE_MODE = INITIAL_BASE_MODE
   FRACTION_MODE = INITIAL_FRACTION_MODE

case('DEG')                                                ! DEG
   ANGLE_MODE = 1
   ANGLE_FACTOR = PI/180.0D0

case('DIGAMMA')                                            ! DIGAMMA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = PSI(STACK(1))
      CASE (2)
         write(stderr, *) ' DIGAMMA function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = PSI(STACK(1))
   END SELECT

case('DUP')                                                ! DUP
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK(STACK(1))
      CASE (2)
         CALL push_stack(CSTACK(1))
      CASE (3)
         CALL push_stack(RNSTACK(1),RDSTACK(1))
   END SELECT

case('ECHG')                                               ! ECHG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (ECHG)
      CASE (2)
         CALL push_stack(CMPLX(ECHG, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (ECHG)
   END SELECT

case('EPS0')                                               ! EPS0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (EPS0)
      CASE (2)
         CALL push_stack(CMPLX(EPS0, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (EPS0)
   END SELECT

case('ERF')                                                ! ERF
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = erf(STACK(1))
      CASE (2)
         write(stderr, *) ' ERF function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = erf(STACK(1))
   END SELECT

case('ERFC')                                               ! ERFC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = erfc(STACK(1))
      CASE (2)
         write(stderr, *) ' ERFC function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = erfc(STACK(1))
   END SELECT

case('EULER')                                             ! EULER
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (EULER)
      CASE (2)
         CALL push_stack(CMPLX(EULER, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (EULER)
   END SELECT

case('EXP')                                                ! EXP
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = EXP(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = EXP(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = EXP(STACK(1))
   END SELECT

case('EXSEC')                                             ! EXSEC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = EXSEC(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CEXSEC(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = EXSEC(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('F>C')                                                ! F>C
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = (5.0D0/9.0D0)*(STACK(1)-32.0D0)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = (5.0D0/9.0D0)*(CSTACK(1)-32.0D0)
      CASE (3)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         CALL RSUB (RNSTACK(1),RDSTACK(1),32,1,NUM,DEN)
         CALL RMUL (5,9,NUM,DEN,NUM2,DEN2)
         RNSTACK(1) = NUM2
         RDSTACK(1) = DEN2
   END SELECT

case('FRAC')                                               ! FRAC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = FRAC(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = FRAC(CSTACK(1))
      CASE (3)
         CALL RFRAC(RNSTACK(1),RDSTACK(1),NUM,DEN)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM
         RDSTACK(1) = DEN
   END SELECT

case('FRACTOL')                                            ! FRACTOL
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         FRACTOL = STACK(1)
         CALL DROP_STACK(1)
      CASE (2)
         FRACTOL = real(CSTACK(1), wp)
         CALL CDROP_STACK(1)
      CASE (3)
         FRACTOL = real(RNSTACK(1), wp)/real(RDSTACK(1), wp)
         CALL RDROP_STACK(1)
   END SELECT

case('G')                                                  ! G
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (G)
      CASE (2)
         CALL push_stack(CMPLX(G, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (G)
   END SELECT

case('GAL>L')                                             ! GAL>L
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1) * L_PER_GAL
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1) * L_PER_GAL
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(1) * L_PER_GAL
   END SELECT

case('GAMMA')                                             ! GAMMA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISINT(STACK(1)).AND.(STACK(1)<=0.0D0)) THEN
            write(stderr, *) '  GAMMA Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = gamma(STACK(1))
         END IF
      CASE (2)
         IF (isclose(cstack(1), C0)) THEN
            write(stderr, *) '  GAMMA Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CGAMMA(CSTACK(1))
         END IF
      CASE (3)
         IF ((RDSTACK(1)==1).AND.(RNSTACK(1)<=0)) THEN
            write(stderr, *) '  GAMMA Error'
         ELSE
            IF (RDSTACK(1)==1) THEN
               ITMP = RNSTACK(1)
               IF (ITMP<=0) THEN
                  write(stderr, *) '  GAMMA Error'
               ELSE
                  ITMP2 = 1
                  DO I = 2, ITMP-1
                     ITMP2 = ITMP2 * I
                  END DO
                  RNLASTX = RNSTACK(1)
                  RDLASTX = RDSTACK(1)
                  RNSTACK(1) = ITMP2
                  RDSTACK(1) = 1
               END IF
            ELSE
               CALL SWITCH_RAT_TO_REAL
               IF (ISINT(STACK(1)).AND.(STACK(1)<=0.0D0)) THEN
                  write(stderr, *) '  GAMMA Error'
               ELSE
                  LASTX = STACK(1)
                  STACK(1) = gamma(STACK(1))
               END IF
            END IF
         END IF
   END SELECT

case('GCD')                                                ! GCD
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISFRAC(STACK(1)).OR.ISFRAC(STACK(2))) THEN
            write(stderr, *) '  GCD Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = GCD(NINT(STACK(2)),NINT(STACK(1)))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)).OR.ISFRAC(real(CSTACK(2), wp)).OR. &
            .not.isclose(AIMAG(CSTACK(1)), 0._wp).OR..not.isclose(AIMAG(CSTACK(2)), 0._wp)) THEN
            write(stderr, *) '  GCD Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = GCD(NINT(real(CSTACK(2), wp)),NINT(real(CSTACK(1), wp)))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF ((RDSTACK(1)/=1).OR.(RDSTACK(2)/=1)) THEN
            write(stderr, *) '  GCD Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(2)
            RNSTACK(1) = GCD(RNSTACK(2),RNSTACK(1))
            RDSTACK(1) = 1
            CALL RDROP_STACK(2)
         END IF
   END SELECT

case('GOLDEN')                                             ! GOLDEN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (GOLDEN)
      CASE (2)
         CALL push_stack(CMPLX(GOLDEN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (GOLDEN)
   END SELECT

case('GRAD')                                               ! GRAD
   ANGLE_MODE = 3
   ANGLE_FACTOR = PI/200.0D0

case('GRAV')                                               ! GRAV
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (GRAV)
      CASE (2)
         CALL push_stack(CMPLX(GRAV, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (GRAV)
   END SELECT

case('H')                                                  ! H
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (H)
      CASE (2)
         CALL push_stack(CMPLX(H, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (H)
   END SELECT

case('H>HMS')                                             ! H>HMS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL H2HMSD (STACK(1), ITMP, ITMP2, TMP)
         LASTX = STACK(1)
         STACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
      CASE (2)
         IF (.not.isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            write(stderr, *) '  H>HMS Error'
         ELSE
            CALL H2HMSD (real(CSTACK(1), wp), ITMP, ITMP2, TMP)
            CLASTX = CSTACK(1)
            CSTACK(1) = CMPLX(real(itmp, wp)+1.0D-2*ITMP2+1.0D-4*TMP, 0._wp, 8)
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL H2HMSD (STACK(1), ITMP, ITMP2, TMP)
         LASTX = STACK(1)
         STACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
   END SELECT

case('HBAR')                                               ! HBAR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (HBAR)
      CASE (2)
         CALL push_stack(CMPLX(HBAR, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (HBAR)
   END SELECT

case('HEX')                                                ! HEX
   BASE_MODE = 16

case('HMS>H')                                             ! HMS>H
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         ITMP = INT(STACK(1))
         ITMP2 = INT(FRAC(STACK(1))*1.0D2)
         TMP = (STACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
         LASTX = STACK(1)
         STACK(1) = TMP2
      CASE (2)
         IF (.not.isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            write(stderr, *) '  HMS>H Error'
         ELSE
            ITMP = INT(real(CSTACK(1), wp))
            ITMP2 = INT(FRAC(real(CSTACK(1), wp))*1.0D2)
            TMP = (real(CSTACK(1), wp) - ITMP - ITMP2*1.0D-2)*1.0D4
            CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
            CLASTX = CSTACK(1)
            CSTACK(1) = CMPLX(TMP2, 0._wp, wp)
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         ITMP = INT(STACK(1))
         ITMP2 = INT(FRAC(STACK(1))*1.0D2)
         TMP = (STACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
         LASTX = STACK(1)
         STACK(1) = TMP2
   END SELECT

case('HMS+')                                               ! HMS+
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         ITMP = INT(STACK(1))
         ITMP2 = INT(FRAC(STACK(1))*1.0D2)
         TMP = (STACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
         ITMP = INT(STACK(2))
         ITMP2 = INT(FRAC(STACK(2))*1.0D2)
         TMP = (STACK(2) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP3)
         CALL H2HMSD (TMP2+TMP3, ITMP, ITMP2, TMP)
         LASTX = STACK(1)
         STACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
         CALL DROP_STACK(2)
      CASE (2)
         IF (.not.isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            write(stderr, *) '  HMS+ Error'
         ELSE
            ITMP = INT(CSTACK(1))
            ITMP2 = INT(FRAC(CSTACK(1))*1.0e2_wp)
            TMP = (real(CSTACK(1), wp) - ITMP - ITMP2*1.0D-2)*1.0D4
            CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
            ITMP = INT(CSTACK(2))
            ITMP2 = INT(FRAC(CSTACK(2))*1.0e2_wp)
            TMP = (real(CSTACK(2), wp) - ITMP - ITMP2*1.0D-2)*1.0D4
            CALL HMS2H (ITMP, ITMP2, TMP, TMP3)
            CALL H2HMSD (TMP2+TMP3, ITMP, ITMP2, TMP)
            CLASTX = CSTACK(1)
            CSTACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         ITMP = INT(STACK(1))
         ITMP2 = INT(FRAC(STACK(1))*1.0D2)
         TMP = (STACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
         ITMP = INT(STACK(2))
         ITMP2 = INT(FRAC(STACK(2))*1.0D2)
         TMP = (STACK(2) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP3)
         CALL H2HMSD (TMP2+TMP3, ITMP, ITMP2, TMP)
         LASTX = STACK(1)
         STACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
         CALL DROP_STACK(2)
   END SELECT

case('HMS-')                                               ! HMS-
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         ITMP = INT(STACK(1))
         ITMP2 = INT(FRAC(STACK(1))*1.0D2)
         TMP = (STACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
         ITMP = INT(STACK(2))
         ITMP2 = INT(FRAC(STACK(2))*1.0D2)
         TMP = (STACK(2) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP3)
         CALL H2HMSD (TMP3-TMP2, ITMP, ITMP2, TMP)
         LASTX = STACK(1)
         STACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
         CALL DROP_STACK(2)
      CASE (2)
         IF (.not.isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            write(stderr, *) '  HMS- Error'
         ELSE
            ITMP = INT(CSTACK(1))
            ITMP2 = INT(FRAC(CSTACK(1))*1.0D2)
            TMP = (real(CSTACK(1), wp) - ITMP - ITMP2*1.0D-2)*1.0D4
            CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
            ITMP = INT(CSTACK(2))
            ITMP2 = INT(FRAC(CSTACK(2))*1.0D2)
            TMP = (real(CSTACK(2), wp) - ITMP - ITMP2*1.0D-2)*1.0D4
            CALL HMS2H (ITMP, ITMP2, TMP, TMP3)
            CALL H2HMSD (TMP3-TMP2, ITMP, ITMP2, TMP)
            CLASTX = CSTACK(1)
            CSTACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         ITMP = INT(STACK(1))
         ITMP2 = INT(FRAC(STACK(1))*1.0D2)
         TMP = (STACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
         ITMP = INT(STACK(2))
         ITMP2 = INT(FRAC(STACK(2))*1.0D2)
         TMP = (STACK(2) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP3)
         CALL H2HMSD (TMP3-TMP2, ITMP, ITMP2, TMP)
         LASTX = STACK(1)
         STACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
         CALL DROP_STACK(2)
   END SELECT

case('HAV')                                                ! HAV
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = HAV(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = HAV(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = HAV(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('HYPOT')                                             ! HYPOT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SQRT(STACK(1)**2+STACK(2)**2)
         CALL DROP_STACK(2)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = SQRT(CSTACK(1)**2+CSTACK(2)**2)
         CALL CDROP_STACK(2)
      CASE (3)
         NUM = RNSTACK(1)
         DEN = RDSTACK(1)
         NUM2 = RNSTACK(2)
         DEN2 = RDSTACK(2)
         CALL RMUL (NUM,DEN,NUM,DEN,NUM3,DEN3)
         CALL RMUL (NUM2,DEN2,NUM2,DEN2,NUM4,DEN4)
         CALL RADD (NUM3,DEN3,NUM4,DEN4,NUM,DEN)
         TMP = SQRT(real(NUM, wp))
         TMP2 = SQRT(real(DEN, wp))
         IF (ISFRAC(TMP).OR.ISFRAC(TMP2)) THEN
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = SQRT(STACK(1)**2+STACK(2)**2)
            CALL DROP_STACK(2)
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = NINT(SQRT(real(NUM, wp)))
            RDSTACK(1) = NINT(SQRT(real(DEN, wp)))
            CALL RDROP_STACK(2)
         END IF
   END SELECT

case('HYPOT3')                                             ! HYPOT3
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SQRT(STACK(1)**2+STACK(2)**2+STACK(3)**2)
         CALL DROP_STACK(3)
         CALL DROP_STACK(2)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = SQRT(CSTACK(1)**2+CSTACK(2)**2+CSTACK(3)**2)
         CALL CDROP_STACK(3)
         CALL CDROP_STACK(2)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = SQRT(STACK(1)**2+STACK(2)**2+STACK(3)**2)
         CALL DROP_STACK(3)
         CALL DROP_STACK(2)
   END SELECT

case('I')                                                  ! I
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         write(stderr, *) ' i not available in REAL mode'
      CASE (2)
         CALL push_stack(II)
      CASE (3)
         write(stderr, *) ' i not available in RATIONAL mode'
   END SELECT

case('IM')                                                 ! IM
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = 0._wp
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(AIMAG(CSTACK(1)), 0._wp, wp)
      CASE (3)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = 0
         RDSTACK(1) = 1
   END SELECT

case('IMPROPER')                                           ! IMPROPER
   FRACTION_MODE = 1

case('IN>CM')                                             ! IN>CM
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1) * CM_PER_IN
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1) * CM_PER_IN
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(1) * CM_PER_IN
   END SELECT

case('INT')                                                ! INT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = AINT(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CINT(CSTACK(1))
      CASE (3)
         NUM = RINT(RNSTACK(1),RDSTACK(1))
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM
         RDSTACK(1) = 1
   END SELECT

case('INT/')                                               ! INT/
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(stack(1), 0._wp)) THEN
            write(stderr, *) '  INT/ Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AINT(STACK(2) / STACK(1))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (isclose(cstack(1), C0)) THEN
            write(stderr, *) '  INT/ Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CINT(CSTACK(2) / CSTACK(1))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF (RNSTACK(1) == 0) THEN
            write(stderr, *) '  INT/ Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = RNSTACK(1) / RDSTACK(1)
            RDSTACK(1) = 1
            CALL RDROP_STACK(2)
         END IF
   END SELECT

case('KB')                                                 ! KB
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (KB)
      CASE (2)
         CALL push_stack(CMPLX(KB, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (KB)
   END SELECT

case('KEPLER')                                             ! KEPLER
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = KEPLER(STACK(2)*ANGLE_FACTOR,STACK(1))/ANGLE_FACTOR
      CASE (2)
         TMP = KEPLER(real(CSTACK(2), wp)*ANGLE_FACTOR,real(CSTACK(1), wp)) / &
            ANGLE_FACTOR
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP, 0._wp, wp)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = KEPLER(STACK(2)*ANGLE_FACTOR,STACK(1))/ANGLE_FACTOR
   END SELECT

case('KG>LB')                                             ! KG>LB
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1) / KG_PER_LB
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1) / KG_PER_LB
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(1) / KG_PER_LB
   END SELECT

case('L>GAL')                                             ! L>GAL
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1) / L_PER_GAL
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1) / L_PER_GAL
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(1) / L_PER_GAL
   END SELECT

case('LASTX')                                             ! LASTX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (LASTX)
      CASE (2)
         CALL push_stack(CLASTX)
      CASE (3)
         CALL push_stack(RNLASTX, RDLASTX)
   END SELECT

case('LB>KG')                                             ! LB>KG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1) * KG_PER_LB
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1) * KG_PER_LB
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(1) * KG_PER_LB
   END SELECT

case('LCM')                                                ! LCM
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISFRAC(STACK(1)).OR.ISFRAC(STACK(2))) THEN
            write(stderr, *) '  LCM Error'
         ELSE IF (isclose(STACK(1), 0._wp).AND.isclose(STACK(2), 0._wp)) THEN
            write(stderr, *) '  LCM Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LCM(NINT(STACK(2)),NINT(STACK(1)))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)).OR.ISFRAC(real(CSTACK(2), wp)).OR. &
            .not.isclose(AIMAG(CSTACK(1)), 0._wp).OR..not.isclose(AIMAG(CSTACK(2)), 0._wp)) THEN
            write(stderr, *) '  LCM Error'
         ELSEIF (isclose(cstack(1), C0) .AND. isclose(cstack(2), C0)) THEN
            write(stderr, *) '  LCM Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = LCM(NINT(real(CSTACK(2), wp)),NINT(real(CSTACK(1), wp)))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF ((RDSTACK(1)/=1).OR.(RDSTACK(2)/=1)) THEN
            write(stderr, *) '  LCM Error'
         ELSE IF ((RNSTACK(1)==0).AND.(RNSTACK(2)==0)) THEN
            write(stderr, *) '  LCM Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(2)
            RNSTACK(1) = LCM(RNSTACK(2),RNSTACK(1))
            RDSTACK(1) = 1
            CALL RDROP_STACK(2)
         END IF
   END SELECT

case('LN')                                                 ! LN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  LN Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1))
         END IF
      CASE (2)
         IF (isclose(cstack(1), C0)) THEN
            write(stderr, *) '  LN Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = LOG(CSTACK(1))
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  LN Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1))
         END IF
   END SELECT

case('LOG')                                                ! LOG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  LOG Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG10(STACK(1))
         END IF
      CASE (2)
         IF (isclose(cstack(1), C0)) THEN
            write(stderr, *) '  LOG Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CLOG10(CSTACK(1))
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  LOG Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG10(STACK(1))
         END IF
   END SELECT

case('LOG2')                                               ! LOG2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  LOG2 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1)) / log(2._wp)
         END IF
      CASE (2)
         IF (isclose(cstack(1), C0)) THEN
            write(stderr, *) '  LOG2 Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = LOG(CSTACK(1)) / log(2._wp)
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) <= 0._wp) THEN
            write(stderr, *) '  LOG2 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1)) / log(2._wp)
         END IF
   END SELECT

case('LR')                                                 ! LR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  LR Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            CALL PUSH_STACK (TMPM)
            CALL PUSH_STACK (TMPB)
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  LR Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CALL push_stack(CTMPM)
            CALL push_stack(CTMPB)
         END IF
      CASE (3)
         IF (RNNN <= 1) THEN
            write(stderr, *) '  LR Error'
         ELSE
            CALL RLINREG (NUMM,DENM,NUMB,DENB,TMPR)
            CALL push_stack(NUMM,DENM)
            CALL push_stack(NUMB,DENB)
         END IF
   END SELECT

case('ME')                                                 ! ME
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (ME)
      CASE (2)
         CALL push_stack(CMPLX(ME, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (ME)
   END SELECT

case('MIXED')                                             ! MIXED
   FRACTION_MODE = 2

case('MN')                                                 ! MN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MN)
      CASE (2)
         CALL push_stack(CMPLX(MN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MN)
   END SELECT

case('MOD')                                                ! MOD
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(stack(1), 0._wp)) THEN
            write(stderr, *) '  MOD Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = MOD (STACK(2),STACK(1))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (isclose(cstack(1), C0)) THEN
            write(stderr, *) '  MOD Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CMOD (CSTACK(2),CSTACK(1))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF (RNSTACK(1) == 0) THEN
            write(stderr, *) '  MOD Error'
         ELSE
            NUM = RNSTACK(1)
            DEN = RDSTACK(1)
            NUM2 = RNSTACK(2)
            DEN2 = RDSTACK(2)
            CALL RMOD (NUM2, DEN2, NUM, DEN, ITMP, ITMP2)
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = ITMP
            RDSTACK(1) = ITMP2
            CALL RDROP_STACK(2)
         END IF
   END SELECT

case('MODES')                                             ! MODES
   WRITE (UNIT=*, FMT='()')
   SELECT CASE (ANGLE_MODE)
      CASE (1)
         print *, '  Angles:     DEG'
      CASE (2)
         print *, '  Angles:     RAD'
      CASE (3)
         print *, '  Angles:     GRAD'
      CASE (4)
         print *, '  Angles:     REV'
   END SELECT
   SELECT CASE (DISP_MODE)
      CASE (1)
        print '(A,I0)', '  Display:    FIX ', DISP_DIGITS
      CASE (2)
        print '(A,I0)', '  Display:    SCI ', DISP_DIGITS
      CASE (3)
        print '(A,I0)', '  Display:    ENG ', DISP_DIGITS
      CASE (4)
         print *, '  Display:    ALL '
   END SELECT
   SELECT CASE (BASE_MODE)
      CASE (2)
         print *, '  Base:       BIN'
      CASE (8)
         print *, '  Base:       OCT'
      CASE (10)
         print *, '  Base:       DEC'
      CASE (16)
         print *, '  Base:       HEX'
   END SELECT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         print *, '  Domain:     REAL'
      CASE (2)
         print *, '  Domain:     COMPLEX'
      CASE (3)
         print *, '  Domain:     RATIONAL'
   END SELECT
   SELECT CASE (FRACTION_MODE)
      CASE (1)
         print *, '  Fractions:  IMPROPER'
      CASE (2)
         print *, '  Fractions:  MIXED'
   END SELECT
   print *, ' '

case('MP')                                                 ! MP
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MP)
      CASE (2)
         CALL push_stack(CMPLX(MP, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MP)
   END SELECT

case('MU0')                                                ! MU0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MU0)
      CASE (2)
         CALL push_stack(CMPLX(MU0, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MU0)
   END SELECT

case('MUB')                                                ! MUB
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MUB)
      CASE (2)
         CALL push_stack(CMPLX(MUB, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MUB)
   END SELECT

case('MUN')                                                ! MUN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MUN)
      CASE (2)
         CALL push_stack(CMPLX(MUN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MUN)
   END SELECT

case('N')                                                  ! N
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (NN)
      CASE (2)
         CALL push_stack(CNN)
      CASE (3)
         CALL push_stack(RNNN, RDNN)
   END SELECT

case('NA')                                                 ! NA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (NA)
      CASE (2)
         CALL push_stack(CMPLX(NA, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (NA)
   END SELECT

case('NOT')                                                ! NOT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = NOT (INT(STACK(1)))
      CASE (2)
         TMP = NOT (INT(real(CSTACK(1), wp)))
         TMP2 = NOT (INT(AIMAG(CSTACK(1))))
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
      CASE (3)
         ITMP = RNSTACK(1)/RDSTACK(1)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NOT (ITMP)
         RDSTACK(1) = 1
   END SELECT

case('OCT')                                                ! OCT
   BASE_MODE = 8

case('OR')                                                 ! OR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = IOR (INT(STACK(2)), INT(STACK(1)))
         CALL DROP_STACK(2)
      CASE (2)
         TMP = IOR (INT(real(CSTACK(2), wp)), INT(real(CSTACK(1), wp)))
         TMP2 = IOR (INT(AIMAG(CSTACK(2))), INT(AIMAG(CSTACK(1))))
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
         CALL CDROP_STACK(2)
      CASE (3)
         ITMP = RNSTACK(1)/RDSTACK(1)
         ITMP2 = RNSTACK(2)/RDSTACK(2)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = IOR (ITMP2, ITMP)
         RDSTACK(1) = 1
         CALL RDROP_STACK(2)
   END SELECT

case('P>R')                                                ! P>R
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         TMP  = STACK(1)*COS(STACK(2)*ANGLE_FACTOR)
         TMP2 = STACK(1)*SIN(STACK(2)*ANGLE_FACTOR)
         LASTX = STACK(1)
         STACK(1) = TMP
         STACK(2) = TMP2
      CASE (2)
         TMP  = real(CSTACK(1), wp)*COS(AIMAG(CSTACK(1))*ANGLE_FACTOR)
         TMP2 = real(CSTACK(1), wp)*SIN(AIMAG(CSTACK(1))*ANGLE_FACTOR)
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         TMP  = STACK(1)*COS(STACK(2)*ANGLE_FACTOR)
         TMP2 = STACK(1)*SIN(STACK(2)*ANGLE_FACTOR)
         LASTX = STACK(1)
         STACK(1) = TMP
         STACK(2) = TMP2
   END SELECT

case('PI')                                                 ! PI
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (PI)
      CASE (2)
         CALL push_stack(CMPLX(PI, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (PI)
   END SELECT

case('PNR')                                                ! PNR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((RDSTACK(1)/=1).OR.(RDSTACK(2)/=1)) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF ((RNSTACK(1)<0) .OR. (RNSTACK(2)<0)) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF (RNSTACK(2) < RNSTACK(1)) THEN
            write(stderr, *) '  PNR Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = PNR (ITMP2, ITMP)
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)) .OR. ISFRAC(real(CSTACK(2), wp))) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF (real(CSTACK(1), wp)<0.0D0) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF (real(CSTACK(2), wp)<0.0D0) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF (.not.isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF (.not.isclose(AIMAG(CSTACK(2)), 0._wp)) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF (real(CSTACK(2), wp) < real(CSTACK(1), wp)) THEN
            write(stderr, *) '  PNR Error'
         ELSE
            ITMP  = NINT(real(CSTACK(1), wp))
            ITMP2 = NINT(real(CSTACK(2), wp))
            TMP = PNR (ITMP2, ITMP)
            CLASTX = CSTACK(1)
            CSTACK(1) = CMPLX(TMP, 0._wp, wp)
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF (ISFRAC(STACK(1)) .OR. ISFRAC(STACK(2))) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF ((STACK(1)<0.0D0) .OR. (STACK(2)<0.0D0)) THEN
            write(stderr, *) '  PNR Error'
         ELSE IF (STACK(2) < STACK(1)) THEN
            write(stderr, *) '  PNR Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = int(PNR(RNSTACK(2), RNSTACK(1)))
            CALL RDROP_STACK(2)
         END IF
   END SELECT

case('PR')                                                 ! PR
   WRITE (UNIT=*, FMT='()')
   DO I = 0, REG_SIZE-1
      SELECT CASE (DOMAIN_MODE)
         CASE (1)
            CALL PRINTX(REG(I), NUMSTR)
         CASE (2)
            CALL printx(CREG(I), NUMSTR)
         CASE (3)
            CALL printx(RNREG(I), RDREG(I), NUMSTR)
      END SELECT
      WRITE (UNIT=*, FMT='(1X,I3,A)') I, ':  '//TRIM(NUMSTR)
   END DO
   print *, ' '

case('PS')                                                 ! PS
   WRITE (UNIT=*, FMT='()')
   DO I = STACK_SIZE, 1, -1
      SELECT CASE (I)
         CASE (1)
            REGNAME = ' X'
         CASE (2)
            REGNAME = ' Y'
         CASE (3)
            REGNAME = ' Z'
         CASE (4)
            REGNAME = ' T'
         CASE DEFAULT
            WRITE (UNIT=REGNAME, FMT='(I2)') I
      END SELECT
      SELECT CASE (DOMAIN_MODE)
         CASE (1)
            CALL PRINTX(STACK(I), NUMSTR)
         CASE (2)
            CALL printx(CSTACK(I), NUMSTR)
         CASE (3)
            CALL printx(RNSTACK(I), RDSTACK(I), NUMSTR)
      END SELECT
      WRITE (UNIT=*, FMT='(2X,A)') REGNAME//':  '//TRIM(NUMSTR)
   END DO
   print *, ' '

case('PSUMS')                                             ! PSUMS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='()')
         CALL PRINTX(NN, NUMSTR)
         print *, '  N:   '//TRIM(NUMSTR)
         CALL PRINTX(SUMX, NUMSTR)
         print *, '  X:   '//TRIM(NUMSTR)
         CALL PRINTX(SUMX2, NUMSTR)
         print *, '  X2:  '//TRIM(NUMSTR)
         CALL PRINTX(SUMY, NUMSTR)
         print *, '  Y:   '//TRIM(NUMSTR)
         CALL PRINTX(SUMY2, NUMSTR)
         print *, '  Y2:  '//TRIM(NUMSTR)
         CALL PRINTX(SUMXY, NUMSTR)
         print *, '  XY:  '//TRIM(NUMSTR)
         print *, ' '
      CASE (2)
         WRITE (UNIT=*, FMT='()')
         CALL printx(CNN, NUMSTR)
         print *, '  N:   '//TRIM(NUMSTR)
         CALL printx(CSUMX, NUMSTR)
         print *, '  X:   '//TRIM(NUMSTR)
         CALL printx(CSUMX2, NUMSTR)
         print *, '  X2:  '//TRIM(NUMSTR)
         CALL printx(CSUMY, NUMSTR)
         print *, '  Y:   '//TRIM(NUMSTR)
         CALL printx(CSUMY2, NUMSTR)
         print *, '  Y2:  '//TRIM(NUMSTR)
         CALL printx(CSUMXY, NUMSTR)
         print *, '  XY:  '//TRIM(NUMSTR)
         print *, ' '
      CASE (3)
         WRITE (UNIT=*, FMT='()')
         CALL printx(RNNN, RDNN, NUMSTR)
         print *, '  N:   '//TRIM(NUMSTR)
         CALL printx(RNSUMX, RDSUMX, NUMSTR)
         print *, '  X:   '//TRIM(NUMSTR)
         CALL printx(RNSUMX2, RDSUMX2, NUMSTR)
         print *, '  X2:  '//TRIM(NUMSTR)
         CALL printx(RNSUMY, RDSUMY, NUMSTR)
         print *, '  Y:   '//TRIM(NUMSTR)
         CALL printx(RNSUMY2, RDSUMY2, NUMSTR)
         print *, '  Y2:  '//TRIM(NUMSTR)
         CALL printx(RNSUMXY, RDSUMXY, NUMSTR)
         print *, '  XY:  '//TRIM(NUMSTR)
         print *, ' '
   END SELECT

case('R')                                                  ! R
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         stack = cshift(stack, 1)
      CASE (2)
         cstack = cshift(cstack, 1)
      CASE (3)
         rnstack = cshift(rnstack, 1)
         rdstack = cshift(rdstack, 1)
   END SELECT
   
case('D')                                                  ! D
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         call drop_stack(1)
      CASE (2)
         call cdrop_stack(1)
      CASE (3)
         call rdrop_stack(1)
   END SELECT

case('R>D')                                                ! R>D
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1)*180.0D0/PI
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1)*180.0D0/PI
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(1)*180.0D0/PI
   END SELECT

case('R>P')                                                ! R>P
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         TMP  = SQRT((STACK(1))**2+(STACK(2))**2)
         TMP2 = ATAN2(STACK(2),STACK(1))/ANGLE_FACTOR
         LASTX = STACK(1)
         STACK(1) = TMP
         STACK(2) = TMP2
      CASE (2)
         TMP = ABS(CSTACK(1))
         TMP2 = ATAN2(AIMAG(CSTACK(1)),real(CSTACK(1), wp))/ANGLE_FACTOR
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         TMP  = SQRT((STACK(1))**2+(STACK(2))**2)
         TMP2 = ATAN2(STACK(2),STACK(1))/ANGLE_FACTOR
         LASTX = STACK(1)
         STACK(1) = TMP
         STACK(2) = TMP2
   END SELECT

case('RAD')                                                ! RAD
   ANGLE_MODE = 2
   ANGLE_FACTOR = 1._wp

case('RAND')                                               ! RAND
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL RANDOM_NUMBER (TMP)
         CALL PUSH_STACK(TMP)
      CASE (2)
         CALL RANDOM_NUMBER (TMP)
         CALL RANDOM_NUMBER (TMP2)
         CALL push_stack(CMPLX(TMP,TMP2, wp))
      CASE (3)
         CALL RANDOM_NUMBER (TMP)
         CALL DEC_TO_FRAC (TMP, NUM, DEN, FRACTOL)
         CALL push_stack(NUM, DEN)
   END SELECT

case('RATIONAL')                                           ! RATIONAL
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         DOMAIN_MODE = 3
         DO I = 1, STACK_SIZE
            CALL DEC_TO_FRAC (STACK(I),ITMP,ITMP2,FRACTOL)
            RNSTACK(I) = ITMP
            RDSTACK(I) = ITMP2
         END DO
         DO I = 0, REG_SIZE-1
            CALL DEC_TO_FRAC (REG(I),ITMP,ITMP2,FRACTOL)
            RNREG(I) = ITMP
            RDREG(I) = ITMP2
         END DO
         CALL DEC_TO_FRAC (LASTX,ITMP,ITMP2,FRACTOL)
         RNLASTX = ITMP
         RDLASTX = ITMP2
         CALL DEC_TO_FRAC (NN,ITMP,ITMP2,FRACTOL)
         RNNN = ITMP
         RDNN = ITMP2
         CALL DEC_TO_FRAC (SUMX,ITMP,ITMP2,FRACTOL)
         RNSUMX = ITMP
         RDSUMX = ITMP2
         CALL DEC_TO_FRAC (SUMX2,ITMP,ITMP2,FRACTOL)
         RNSUMX2 = ITMP
         RDSUMX2 = ITMP2
         CALL DEC_TO_FRAC (SUMY,ITMP,ITMP2,FRACTOL)
         RNSUMY = ITMP
         RDSUMY = ITMP2
         CALL DEC_TO_FRAC (SUMY2,ITMP,ITMP2,FRACTOL)
         RNSUMY2 = ITMP
         RDSUMY2 = ITMP2
         CALL DEC_TO_FRAC (SUMXY,ITMP,ITMP2,FRACTOL)
         RNSUMXY = ITMP
         RDSUMXY = ITMP2
      CASE (2)
         DOMAIN_MODE = 3
         DO I = 1, STACK_SIZE
            CALL DEC_TO_FRAC (real(CSTACK(I), wp),ITMP,ITMP2,FRACTOL)
            RNSTACK(I) = ITMP
            RDSTACK(I) = ITMP2
         END DO
         DO I = 0, REG_SIZE-1
            CALL DEC_TO_FRAC (real(CREG(I), wp),ITMP,ITMP2,FRACTOL)
            RNREG(I) = ITMP
            RDREG(I) = ITMP2
         END DO
         CALL DEC_TO_FRAC (real(CLASTX, wp),ITMP,ITMP2,FRACTOL)
         RNLASTX = ITMP
         RDLASTX = ITMP2
         CALL DEC_TO_FRAC (real(CNN, wp),ITMP,ITMP2,FRACTOL)
         RNNN = ITMP
         RDNN = ITMP2
         CALL DEC_TO_FRAC (real(CSUMX, wp),ITMP,ITMP2,FRACTOL)
         RNSUMX = ITMP
         RDSUMX = ITMP2
         CALL DEC_TO_FRAC (real(CSUMX2, wp),ITMP,ITMP2,FRACTOL)
         RNSUMX2 = ITMP
         RDSUMX2 = ITMP2
         CALL DEC_TO_FRAC (real(CSUMY, wp),ITMP,ITMP2,FRACTOL)
         RNSUMY = ITMP
         RDSUMY = ITMP2
         CALL DEC_TO_FRAC (real(CSUMY2, wp),ITMP,ITMP2,FRACTOL)
         RNSUMY2 = ITMP
         RDSUMY2 = ITMP2
         CALL DEC_TO_FRAC (real(CSUMXY, wp),ITMP,ITMP2,FRACTOL)
         RNSUMXY = ITMP
         RDSUMXY = ITMP2
   END SELECT

case('RCORR')                                             ! RCORR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  RCORR Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            CALL PUSH_STACK (TMPR)
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  RCORR Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CALL push_stack(CTMPR)
         END IF
      CASE (3)
         IF (RNNN <= 1) THEN
            write(stderr, *) '  RCORR Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            CALL LINREG (TMPM,TMPB,TMPR)
            CALL PUSH_STACK (TMPR)
         END IF
   END SELECT

case('RE')                                                 ! RE
   SELECT CASE (DOMAIN_MODE)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(real(CSTACK(1), wp), 0._wp, wp)
   END SELECT

case('REAL')                                               ! REAL
   SELECT CASE (DOMAIN_MODE)
      CASE (2)
         DOMAIN_MODE = 1
         
         STACK = real(CSTACK, wp)

         REG = real(CREG, wp)
         
         LASTX = real(CLASTX, wp)
         NN = real(CNN, wp)
         SUMX = real(CSUMX, wp)
         SUMX2 = real(CSUMX2, wp)
         SUMY = real(CSUMY, wp)
         SUMY2 = real(CSUMY2, wp)
         SUMXY = real(CSUMXY, wp)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
   END SELECT

case('REARTH')                                             ! REARTH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (REARTH)
      CASE (2)
         CALL push_stack(CMPLX(REARTH, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (REARTH)
   END SELECT

case('REDUCE')                                             ! REDUCE
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         TMP = STACK(1)
         CALL DROP_STACK(1)
         LASTX = STACK(1)
         STACK(1) = REDUCE(STACK(1)*ANGLE_FACTOR,TMP*ANGLE_FACTOR) &
            / ANGLE_FACTOR
      CASE (2)
         TMP = real(CSTACK(1), wp)
         CALL CDROP_STACK(1)
         CLASTX = CSTACK(1)
         TMP2 = REDUCE(real(CSTACK(1), wp)*ANGLE_FACTOR,TMP*ANGLE_FACTOR) &
            / ANGLE_FACTOR
         CSTACK(1) = CMPLX(TMP2, 0._wp, wp)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         TMP = STACK(1)
         CALL DROP_STACK(1)
         LASTX = STACK(1)
         STACK(1) = REDUCE(STACK(1)*ANGLE_FACTOR,TMP*ANGLE_FACTOR) &
            / ANGLE_FACTOR
   END SELECT

case('RESET')                                             ! RESET
   STACK = 0._wp
   REG = 0._wp
   LASTX = 0._wp

   NN = 0._wp
   SUMX = 0._wp
   SUMX2 = 0._wp
   SUMY = 0._wp
   SUMY2 = 0._wp
   SUMXY = 0._wp

   CSTACK = (0._wp, 0._wp)
   CREG = (0._wp, 0._wp)
   CLASTX = (0._wp, 0._wp)

   CNN = (0._wp, 0._wp)
   CSUMX = (0._wp, 0._wp)
   CSUMX2 = (0._wp, 0._wp)
   CSUMY = (0._wp, 0._wp)
   CSUMY2 = (0._wp, 0._wp)
   CSUMXY = (0._wp, 0._wp)

   RNSTACK = 0; RDSTACK = 1
   RNREG = 0; RDREG = 1
   RNLASTX = 0; RDLASTX = 1

   RNNN = 0; RDNN = 1
   RNSUMX = 0; RDSUMX = 1
   RNSUMX2 = 0; RDSUMX2 = 1
   RNSUMY = 0; RDSUMY = 1
   RNSUMY2 = 0; RDSUMY2 = 1
   RNSUMXY = 0; RDSUMXY = 1

   ANGLE_MODE = INITIAL_ANGLE_MODE

   SELECT CASE (ANGLE_MODE)
      CASE (1)
         ANGLE_FACTOR = PI/180.0D0
      CASE (2)
         ANGLE_FACTOR = 1._wp
      CASE (3)
         ANGLE_FACTOR = PI/200.0D0
      CASE (4)
         ANGLE_FACTOR = 2*pi
   END SELECT

   DISP_MODE = INITIAL_DISP_MODE
   DISP_DIGITS = INITIAL_DISP_DIGITS
   DOMAIN_MODE = INITIAL_DOMAIN_MODE
   BASE_MODE = INITIAL_BASE_MODE
   FRACTION_MODE = INITIAL_FRACTION_MODE

   FRACTOL = INITIAL_FRACTOL

case('REV')                                                ! REV
   ANGLE_MODE = 4
   ANGLE_FACTOR = 2*pi

case('RGAS')                                               ! RGAS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (RGAS)
      CASE (2)
         CALL push_stack(CMPLX(RGAS, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (RGAS)
   END SELECT

case('RI')                                                 ! RI
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK(1) = 0._wp
      CASE (2)
         TMP = real(CSTACK(1), wp)
         TMP2 = AIMAG(CSTACK(1))
         CSTACK(1) = CMPLX(TMP2,TMP, wp)
      CASE (3)
         RNSTACK(1) = 0
         RDSTACK(1) = 1
   END SELECT

case('ROUND')                                             ! ROUND
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = ANINT(STACK(1))
      CASE (2)
         TMP = ANINT(real(CSTACK(1), wp))
         TMP2 = ANINT(AIMAG(CSTACK(1)))
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
      CASE (3)
         NUM = RNSTACK(1)
         DEN = RDSTACK(1)
         CALL RNINT (NUM, DEN)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM
         RDSTACK(1) = DEN
   END SELECT

case('U')                                                ! roll stack up
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         stack = cshift(stack, -1)
      CASE (2)
         cstack = cshift(cstack, -1)
      CASE (3)
         rnstack = cshift(rnstack, -1)
         rdstack = cshift(rdstack, -1)
   END SELECT

case('RZETA')                                             ! RZETA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = RIEMANNZETA(STACK(1), 1.e-10_wp) + 1._wp
      CASE (2)
         write(stderr, *) ' RZETA function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = RIEMANNZETA(STACK(1), 1.e-10_wp) + 1._wp
   END SELECT

case('S')                                                  ! S
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         NN = NN + 1._wp
         SUMX = SUMX + STACK(1)
         SUMX2 = SUMX2 + STACK(1)**2
         SUMY = SUMY + STACK(2)
         SUMY2 = SUMY2 + STACK(2)**2
         SUMXY = SUMXY + STACK(1)*STACK(2)
         LASTX = STACK(1)
         STACK(1) = NN
      CASE (2)
         CNN = CNN + 1._wp
         CSUMX = CSUMX + CSTACK(1)
         CSUMX2 = CSUMX2 + CSTACK(1)**2
         CSUMY = CSUMY + CSTACK(2)
         CSUMY2 = CSUMY2 + CSTACK(2)**2
         CSUMXY = CSUMXY + CSTACK(1)*CSTACK(2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CNN
      CASE (3)
         CALL RADD(RNNN,RDNN,1,1,RNNN,RDNN)
         CALL RADD(RNSUMX,RDSUMX,RNSTACK(1),RDSTACK(1),RNSUMX,RDSUMX)
         CALL RMUL(RNSTACK(1),RDSTACK(1),RNSTACK(1),RDSTACK(1),NUM,DEN)
         CALL RADD(RNSUMX2,RDSUMX2,NUM,DEN,RNSUMX2,RDSUMX2)
         CALL RADD(RNSUMY,RDSUMY,RNSTACK(2),RDSTACK(2),RNSUMY,RDSUMY)
         CALL RMUL(RNSTACK(2),RDSTACK(2),RNSTACK(2),RDSTACK(2),NUM,DEN)
         CALL RADD(RNSUMY2,RDSUMY2,NUM,DEN,RNSUMY2,RDSUMY2)
         CALL RMUL(RNSTACK(2),RDSTACK(2),RNSTACK(1),RDSTACK(1),NUM,DEN)
         CALL RADD(RNSUMXY,RDSUMXY,NUM,DEN,RNSUMXY,RDSUMXY)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = RNNN
         RDSTACK(1) = RDNN
   END SELECT

case('S-')                                                  ! S-
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         NN = NN - 1._wp
         SUMX = SUMX - STACK(1)
         SUMX2 = SUMX2 - STACK(1)**2
         SUMY = SUMY - STACK(2)
         SUMY2 = SUMY2 - STACK(2)**2
         SUMXY = SUMXY - STACK(1)*STACK(2)
         LASTX = STACK(1)
         STACK(1) = NN
      CASE (2)
         CNN = CNN - 1._wp
         CSUMX = CSUMX - CSTACK(1)
         CSUMX2 = CSUMX2 - CSTACK(1)**2
         CSUMY = CSUMY - CSTACK(2)
         CSUMY2 = CSUMY2 - CSTACK(2)**2
         CSUMXY = CSUMXY - CSTACK(1)*CSTACK(2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CNN
      CASE (3)
         CALL RADD(RNNN,RDNN,1,1,RNNN,RDNN)
         CALL RSUB(RNSUMX,RDSUMX,RNSTACK(1),RDSTACK(1),RNSUMX,RDSUMX)
         CALL RMUL(RNSTACK(1),RDSTACK(1),RNSTACK(1),RDSTACK(1),NUM,DEN)
         CALL RSUB(RNSUMX2,RDSUMX2,NUM,DEN,RNSUMX2,RDSUMX2)
         CALL RSUB(RNSUMY,RDSUMY,RNSTACK(2),RDSTACK(2),RNSUMY,RDSUMY)
         CALL RMUL(RNSTACK(2),RDSTACK(2),RNSTACK(2),RDSTACK(2),NUM,DEN)
         CALL RSUB(RNSUMY2,RDSUMY2,NUM,DEN,RNSUMY2,RDSUMY2)
         CALL RMUL(RNSTACK(2),RDSTACK(2),RNSTACK(1),RDSTACK(1),NUM,DEN)
         CALL RSUB(RNSUMXY,RDSUMXY,NUM,DEN,RNSUMXY,RDSUMXY)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = RNNN
         RDSTACK(1) = RDNN
   END SELECT

case('SEC')                                                ! SEC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SEC(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = SEC(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = SEC(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('SECH')                                               ! SECH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SECH(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSECH(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = SECH(STACK(1))
   END SELECT

case('SGN')                                                ! SGN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) < 0._wp) THEN
            TMP = -1._wp
         ELSE IF (isclose(STACK(1), 0._wp)) THEN
            TMP = 0._wp
         ELSE
            TMP = +1._wp
         END IF
         LASTX = STACK(1)
         STACK(1) = TMP
      CASE (2)
         IF (real(CSTACK(1), wp) < 0._wp) THEN
            TMP = -1._wp
         ELSE IF (isclose(real(CSTACK(1), wp), 0._wp)) THEN
            TMP = 0._wp
         ELSE
            TMP = +1._wp
         END IF
         IF (AIMAG(CSTACK(1)) < 0._wp) THEN
            TMP2 = -1._wp
         ELSE IF (isclose(AIMAG(CSTACK(1)), 0._wp)) THEN
            TMP2 = 0._wp
         ELSE
            TMP2 = +1._wp
         END IF
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
      CASE (3)
         IF (RNSTACK(1) < 0) THEN
            ITMP = 1
         ELSE IF (RNSTACK(1) == 0) THEN
            ITMP = 0
         ELSE
            ITMP = +1
         END IF
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = ITMP
         RDSTACK(1) = 1
   END SELECT

case('SIN')                                                ! SIN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SIN(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = SIN(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = SIN(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('SINC')                                               ! SINC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SINC(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSINC(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = SINC(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('SINH')                                               ! SINH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SINH(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = SINH(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = SINH(STACK(1))
   END SELECT

case('SINHC')                                             ! SINHC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = SINHC(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSINHC(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = SINHC(STACK(1))
   END SELECT

case('SQR')                                                ! SQR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(1)**2
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(1)**2
      CASE (3)
         NUM = RNSTACK(1)
         DEN = RDSTACK(1)
         CALL RMUL (NUM,DEN,NUM,DEN,NUM2,DEN2)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = NUM2
         RDSTACK(1) = DEN2
   END SELECT

case('SQRT')                                               ! SQRT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) < 0._wp) THEN
            write(stderr, *) '  SQRT Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = SQRT(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = SQRT(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) < 0) THEN
            write(stderr, *) '  SQRT Error'
         ELSE
            TMP = SQRT(real(RNSTACK(1), wp))
            TMP2 = SQRT(real(RDSTACK(1), wp))
            IF (ISFRAC(TMP).OR.ISFRAC(TMP2)) THEN
               CALL SWITCH_RAT_TO_REAL
               LASTX = STACK(1)
               STACK(1) = SQRT(STACK(1))
            ELSE
               RNLASTX = RNSTACK(1)
               RDLASTX = RDSTACK(1)
               RNSTACK(1) = NINT(SQRT(real(RNSTACK(1), wp)))
               RDSTACK(1) = NINT(SQRT(real(RDSTACK(1), wp)))
            END IF
         END IF
   END SELECT

case('STEFAN')                                             ! STEFAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (STEFAN)
      CASE (2)
         CALL push_stack(CMPLX(STEFAN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (STEFAN)
   END SELECT

case('SUMX')                                               ! SUMX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMX)
      CASE (2)
         CALL push_stack(CSUMX)
      CASE (3)
         CALL push_stack(RNSUMX,RDSUMX)
   END SELECT

case('SUMX2')                                             ! SUMX2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMX2)
      CASE (2)
         CALL push_stack(CSUMX2)
      CASE (3)
         CALL push_stack(RNSUMX2,RDSUMX2)
   END SELECT

case('SUMXY')                                             ! SUMXY
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMXY)
      CASE (2)
         CALL push_stack(CSUMXY)
      CASE (3)
         CALL push_stack(RNSUMXY,RDSUMXY)
   END SELECT

case('SUMY')                                               ! SUMY
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMY)
      CASE (2)
         CALL push_stack(CSUMY)
      CASE (3)
         CALL push_stack(RNSUMY,RDSUMY)
   END SELECT

case('SUMY2')                                             ! SUMY2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMY2)
      CASE (2)
         CALL push_stack(CSUMY2)
      CASE (3)
         CALL push_stack(RNSUMY2,RDSUMY2)
   END SELECT

case('TAN')                                                ! TAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = TAN(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = tan(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = TAN(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('TANC')                                               ! TANC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = TANC(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CTANC(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = TANC(STACK(1)*ANGLE_FACTOR)
   END SELECT

case('TANH')                                               ! TANH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = TANH(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = TANH(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = TANH(STACK(1))
   END SELECT

case('TANHC')                                             ! TANHC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = TANHC(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CTANHC(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = TANHC(STACK(1))
   END SELECT

case('TIME')                                               ! TIME
   CALL DATE_AND_TIME (DATE, TIME, ZONE, DT)
   YEAR = DT(1)
   MONTH = DT(2)
   DAY = DT(3)
   HOUR = DT(5)
   MINUTE = DT(6)
   SECOND = DT(7)
   print '(A,I2.2,A1,I2.2,A1,I4)', '  Date:  ', MONTH,'-', DAY, '-', YEAR
   print '(A,I2.2,A1,I2.2,A1,I2.2)', '  Time:  ', HOUR, ':', MINUTE, ':', SECOND

case('VER')                                                ! VER
   print *, 'Fortran 2008  RPN Calculator.  Version '//VERSION

case('VERS')                                               ! VERS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = VERS(STACK(1)*ANGLE_FACTOR)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CVERS(CSTACK(1)*ANGLE_FACTOR)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = VERS(STACK(1)*ANGLE_FACTOR)
   END SELECT
   
case('X^')                                                 ! X^
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  X^ Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            LASTX = STACK(1)
            STACK(1) = (STACK(1)-TMPB)/TMPM
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  X^ Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CLASTX = CSTACK(1)
            CSTACK(1) = (CSTACK(1)-CTMPB)/CTMPM
         END IF
      CASE (3)
         IF (RNNN <= 1) THEN
            write(stderr, *) '  X^ Error'
         ELSE
            CALL RLINREG (NUMM,DENM,NUMB,DENB,TMPR)
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            CALL RSUB(RNSTACK(1),RDSTACK(1),NUMB,DENB,NUM,DEN)
            CALL RDIV(NUM,DEN,NUMM,DENM,NUM2,DEN2)
            RNSTACK(1) = NUM2
            RDSTACK(1) = DEN2
         END IF
   END SELECT

case('XMEAN')                                             ! XMEAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(NN, 0._wp)) THEN
            write(stderr, *) '  XMEAN Error'
         ELSE
            TMP = SUMX/NN
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (isclose(CNN, C0)) THEN
            write(stderr, *) '  XMEAN Error'
         ELSE
            CTMP = CSUMX/CNN
            CALL push_stack(CTMP)
         END IF
      CASE (3)
         IF (RNNN == 0) THEN
            write(stderr, *) '  XMEAN Error'
         ELSE
            CALL RDIV (RNSUMX,RDSUMX,RNNN,RDNN,NUM,DEN)
            CALL push_stack(NUM,DEN)
         END IF
   END SELECT

case('XOR')                                                ! XOR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = IEOR (INT(STACK(2)), INT(STACK(1)))
         CALL DROP_STACK(2)
      CASE (2)
         TMP = IEOR (INT(real(CSTACK(2), wp)), INT(real(CSTACK(1), wp)))
         TMP2 = IEOR (INT(AIMAG(CSTACK(2))), INT(AIMAG(CSTACK(1))))
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
         CALL CDROP_STACK(2)
      CASE (3)
         ITMP = RNSTACK(1)/RDSTACK(1)
         ITMP2 = RNSTACK(2)/RDSTACK(2)
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = IEOR (ITMP2, ITMP)
         RDSTACK(1) = 1
         CALL RDROP_STACK(2)
   END SELECT

case('XRT')                                                ! XRT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(2) ** (1._wp/STACK(1))
         CALL DROP_STACK(2)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(2) ** (1._wp/CSTACK(1))
         CALL CDROP_STACK(2)
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = STACK(2) ** (1._wp/STACK(1))
         CALL DROP_STACK(2)
   END SELECT

case('XS')                                                 ! XS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  XS Error'
         ELSE
            TMP = SQRT((SUMX2-SUMX**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  XS Error'
         ELSE
            CTMP = SQRT((CSUMX2-CSUMX**2/CNN)/(CNN-1._wp))
            CALL push_stack(CTMP)
         END IF
      CASE (3)
         IF (RNNN <= RDNN) THEN
            write(stderr, *) '  XS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMX2-SUMX**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

case('XSIG')                                               ! XSIG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  XSIG Error'
         ELSE
            TMP = SQRT((SUMX2-SUMX**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  XSIG Error'
         ELSE
            CTMP = SQRT((CSUMX2-CSUMX**2/CNN)/CNN)
            CALL push_stack(CTMP)
         END IF
      CASE (3)
         IF (RNNN <= RDNN) THEN
            write(stderr, *) '  XSIG Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMX2-SUMX**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

case('XY')                                                 ! XY
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         TMP = STACK(1)
         STACK(1) = STACK(2)
         STACK(2) = TMP
      CASE (2)
         CTMP = CSTACK(1)
         CSTACK(1) = CSTACK(2)
         CSTACK(2) = CTMP
      CASE (3)
         ITMP = RNSTACK(1)
         ITMP2 = RDSTACK(1)
         RNSTACK(1) = RNSTACK(2)
         RDSTACK(1) = RDSTACK(2)
         RNSTACK(2) = ITMP
         RDSTACK(2) = ITMP2
   END SELECT

case('Y^')                                                 ! Y^
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  Y^ Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            LASTX = STACK(1)
            STACK(1) = TMPM*STACK(1)+TMPB
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  Y^ Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CLASTX = CSTACK(1)
            CSTACK(1) = CTMPM*CSTACK(1)+CTMPB
         END IF
      CASE (3)
         IF (RNNN <= 1) THEN
            write(stderr, *) '  Y^ Error'
         ELSE
            CALL RLINREG (NUMM,DENM,NUMB,DENB,TMPR)
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            CALL RMUL(NUMM,DENM,RNSTACK(1),RDSTACK(1),NUM,DEN)
            CALL RADD(NUM,DEN,NUMB,DENB,NUM2,DEN2)
            RNSTACK(1) = NUM2
            RDSTACK(1) = DEN2
         END IF
   END SELECT

case('YMEAN')                                             ! YMEAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (isclose(NN, 0._wp)) THEN
            write(stderr, *) '  YMEAN Error'
         ELSE
            TMP = SUMY/NN
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (isclose(CNN, C0)) THEN
            write(stderr, *) '  YMEAN Error'
         ELSE
            CTMP = CSUMY/CNN
            CALL push_stack(CTMP)
         END IF
      CASE (3)
         IF (RNNN == 0) THEN
            write(stderr, *) '  YMEAN Error'
         ELSE
            CALL RDIV (RNSUMY,RDSUMY,RNNN,RDNN,NUM,DEN)
            CALL push_stack(NUM,DEN)
         END IF
   END SELECT

case('YS')                                                 ! YS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  YS Error'
         ELSE
            TMP = SQRT((SUMY2-SUMY**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  YS Error'
         ELSE
            CTMP = SQRT((CSUMY2-CSUMY**2/CNN)/(CNN-1._wp))
            CALL push_stack(CTMP)
         END IF
      CASE (3)
         IF (RNNN <= RDNN) THEN
            write(stderr, *) '  YS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMY2-SUMY**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

case('YSIG')                                               ! YSIG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN <= 1._wp) THEN
            write(stderr, *) '  YSIG Error'
         ELSE
            TMP = SQRT((SUMY2-SUMY**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) <= 1._wp) THEN
            write(stderr, *) '  YSIG Error'
         ELSE
            CTMP = SQRT((CSUMY2-CSUMY**2/CNN)/CNN)
            CALL push_stack(CTMP)
         END IF
      CASE (3)
         IF (RNNN <= RDNN) THEN
            write(stderr, *) '  YSIG Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMY2-SUMY**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

case default
   call regops(str)
END select

END SUBROUTINE EVAL


subroutine regops(str)

character(*), intent(in) :: str
integer :: ierr, itmp

select case(str(:3))

case('RCL')                                                 ! RCL
  IF (LEN_TRIM(STR) == 3) THEN
    write(stderr, *) '  RCLx requires specifying a integer register x ~ (0..9) to store in.  E.g.  RCL6' 
    return
  endif
  
  READ(STR(4:4), '(I1)', IOSTAT=IERR) ITMP
  IF (IERR /= 0) THEN
    write(stderr, *) '  RCL Error with register '//str(4:4)
    return
  endif
  
  IF ((ITMP < 0).OR.(ITMP >= REG_SIZE)) THEN
    write(stderr, *) '  RCL Error: no register at',itmp
    return
  endif
  
  SELECT CASE (DOMAIN_MODE)
    CASE (1)
      CALL PUSH_STACK(REG(ITMP))
    CASE (2)
      CALL push_stack(CREG(ITMP))
    CASE (3)
      CALL push_stack(RNREG(ITMP),RDREG(ITMP))
  END SELECT

  PRINT *, REG(ITMP)

case('STO')                                                 ! STO
  IF (LEN_TRIM(STR) == 3) THEN
    write(stderr, *) '  STOx requires specifying a integer register x ~ (0..9) to store in.  E.g.  STO6' 
    return
  endif
  
  READ (STR(4:4), '(I1)', IOSTAT=IERR) ITMP
  IF (IERR /= 0) THEN
    write(stderr, *) '  STO Error with register '//str(4:4)
    return
  endif
  
  IF ((ITMP < 0).OR.(ITMP >= REG_SIZE)) THEN
    write(stderr, *) '  STO Error: no register at',itmp
    return
  endif
  
  SELECT CASE (DOMAIN_MODE)
    CASE (1)
      REG(ITMP) = STACK(1)
    CASE (2)
      CREG(ITMP) = CSTACK(1)
    CASE (3)
      RNREG(ITMP) = RNSTACK(1)
      RDREG(ITMP) = RDSTACK(1)
  END SELECT
  
  PRINT *, REG(ITMP)
   
case('ENG')                                                 ! ENG
  IF (LEN_TRIM(STR) == 3) THEN
    write(stderr, *) '  ENG Error: must specify # of digits of precision (0..9)'
    return
  endif
  
  READ(STR(4:4), '(I1)', IOSTAT=IERR) ITMP
  IF (IERR /= 0) THEN
    write(stderr, *) '  ENG Error: with # digits: '//str(4:4)
    return
  endif
  
  DISP_MODE = 3
  DISP_DIGITS = ITMP

case('FIX')                                                 ! FIX
  IF (LEN_TRIM(STR) == 3) THEN
    write(stderr, *) '  FIX Error: must specify # of digits of precision (0..9)'
    return
  endif
  READ (UNIT=STR(4:4), FMT=*, IOSTAT=IERR) ITMP
  IF (IERR /= 0) THEN
    write(stderr, *) '  FIX Error: with # digits: '//str(4:4)
    return
  endif
  
  DISP_MODE = 1
  DISP_DIGITS = ITMP

case('SCI')                                                 ! SCI
   IF (LEN_TRIM(STR) == 3) THEN
      write(stderr, *) '  SCI Error: must specify # of digits of precision (0..9)'
    return
  endif
  READ (UNIT=STR(4:4), FMT=*, IOSTAT=IERR) ITMP
  IF (IERR /= 0) THEN
    write(stderr, *) '  SCI Error: with # digits: '//str(4:4)
    return
  endif
  
  DISP_MODE = 2
  DISP_DIGITS = ITMP
  
case default
  WRITE(stderr, *) '  Input error:  "'//TRIM(STR)//'"'
end select


end subroutine regops


subroutine add(mode)
integer, intent(in) :: mode
integer :: NUM, DEN

SELECT CASE (MODE)
  CASE (1)
     LASTX = STACK(1)
     STACK(1) = STACK(2) + STACK(1)
     CALL DROP_STACK(2)
  CASE (2)
     CLASTX = CSTACK(1)
     CSTACK(1) = CSTACK(2) + CSTACK(1)
     CALL CDROP_STACK(2)
  CASE (3)
     CALL RADD (RNSTACK(2),RDSTACK(2),RNSTACK(1),RDSTACK(1),NUM,DEN)
     RNLASTX = RNSTACK(1)
     RDLASTX = RDSTACK(1)
     RNSTACK(1) = NUM
     RDSTACK(1) = DEN
     CALL RDROP_STACK(2)
END SELECT

end subroutine add


subroutine subtract(mode)
integer, intent(in) :: mode
integer :: NUM, DEN

SELECT CASE (MODE)
  CASE (1)
     LASTX = STACK(1)
     STACK(1) = STACK(2) - STACK(1)
     CALL DROP_STACK(2)
  CASE (2)
     CLASTX = CSTACK(1)
     CSTACK(1) = CSTACK(2) - CSTACK(1)
     CALL CDROP_STACK(2)
  CASE (3)
     CALL RSUB (RNSTACK(2),RDSTACK(2),RNSTACK(1),RDSTACK(1),NUM,DEN)
     RNLASTX = RNSTACK(1)
     RDLASTX = RDSTACK(1)
     RNSTACK(1) = NUM
     RDSTACK(1) = DEN
     CALL RDROP_STACK(2)
END SELECT

end subroutine subtract


subroutine multiply(mode)
integer, intent(in) :: mode
integer :: NUM, DEN

SELECT CASE (MODE)
  CASE (1)
     LASTX = STACK(1)
     STACK(1) = STACK(2) * STACK(1)
     CALL DROP_STACK(2)
  CASE (2)
     CLASTX = CSTACK(1)
     CSTACK(1) = CSTACK(2) * CSTACK(1)
     CALL CDROP_STACK(2)
  CASE (3)
     CALL RMUL (RNSTACK(2),RDSTACK(2),RNSTACK(1),RDSTACK(1),NUM,DEN)
     RNLASTX = RNSTACK(1)
     RDLASTX = RDSTACK(1)
     RNSTACK(1) = NUM
     RDSTACK(1) = DEN
     CALL RDROP_STACK(2)
END SELECT

end subroutine multiply


subroutine divide(mode)
integer, intent(in) :: mode
integer :: NUM, DEN

SELECT CASE (MODE)
  CASE (1)
     IF (isclose(stack(1), 0._wp)) THEN
        write(stderr, *) '  Divide by zero Error'
     ELSE
        LASTX = STACK(1)
        STACK(1) = STACK(2) / STACK(1)
        CALL DROP_STACK(2)
     END IF
  CASE (2)
     IF (isclose(cstack(1), C0)) THEN
        write(stderr, *) '  Divide by zero Error'
     ELSE
        CLASTX = CSTACK(1)
        CSTACK(1) = CSTACK(2) / CSTACK(1)
        CALL CDROP_STACK(2)
     END IF
  CASE (3)
     CALL RDIV (RNSTACK(2),RDSTACK(2),RNSTACK(1),RDSTACK(1),NUM,DEN)
     RNLASTX = RNSTACK(1)
     RDLASTX = RDSTACK(1)
     RNSTACK(1) = NUM
     RDSTACK(1) = DEN
     CALL RDROP_STACK(2)
END SELECT

end subroutine divide


subroutine power(mode)
integer, intent(in) :: mode

SELECT CASE (MODE)
  CASE (1)
     LASTX = STACK(1)
     STACK(1) = STACK(2) ** STACK(1)
     CALL DROP_STACK(2)
  CASE (2)
     CLASTX = CSTACK(1)
     CSTACK(1) = CSTACK(2) ** CSTACK(1)
     CALL CDROP_STACK(2)
  CASE (3)
     IF (RDSTACK(1) == 1) THEN
        RNLASTX = RNSTACK(1)
        RDLASTX = RDSTACK(1)
        RNSTACK(1) = RNSTACK(2) ** RNLASTX
        RDSTACK(1) = RDSTACK(2) ** RNLASTX
        CALL RDROP_STACK(2)
     ELSE
        CALL SWITCH_RAT_TO_REAL
        LASTX = STACK(1)
        STACK(1) = STACK(2) ** STACK(1)
        CALL DROP_STACK(2)
     END IF
END SELECT

end subroutine power

end module evals
