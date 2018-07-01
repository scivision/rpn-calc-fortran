module ui
use global
use assert, only: wp
use funcs
implicit none

contains

!***********************************************************************************************************************************
!  PUSH_STACK
!
!  Push a number onto the real stack.
!***********************************************************************************************************************************

SUBROUTINE PUSH_STACK (X)

real(wp), INTENT(IN) :: X

INTEGER :: I


DO I = STACK_SIZE, 2, -1
   STACK(I) = STACK(I-1)
END DO

STACK(1) = X

END SUBROUTINE PUSH_STACK


!***********************************************************************************************************************************
!  CPUSH_STACK
!
!  Push a number onto the complex stack.
!***********************************************************************************************************************************

SUBROUTINE CPUSH_STACK (CX)

COMPLEX(wp), INTENT(IN) :: CX

INTEGER :: I


DO I = STACK_SIZE, 2, -1
   CSTACK(I) = CSTACK(I-1)
END DO

CSTACK(1) = CX

END SUBROUTINE CPUSH_STACK


!***********************************************************************************************************************************
!  RPUSH_STACK
!
!  Push a number onto the rational stack.
!***********************************************************************************************************************************

SUBROUTINE RPUSH_STACK (RN,RD)

INTEGER, INTENT(IN) :: RN,RD

INTEGER :: I


DO I = STACK_SIZE, 2, -1
   RNSTACK(I) = RNSTACK(I-1)
   RDSTACK(I) = RDSTACK(I-1)
END DO

RNSTACK(1) = RN
RDSTACK(1) = RD

END SUBROUTINE RPUSH_STACK


!***********************************************************************************************************************************
!  DROP_STACK
!
!  Drop a number from the real stack.
!***********************************************************************************************************************************

SUBROUTINE DROP_STACK (N)

INTEGER, INTENT(IN) :: N

INTEGER :: I

DO I = N, STACK_SIZE-1
   STACK(I) = STACK(I+1)
END DO

END SUBROUTINE DROP_STACK


!***********************************************************************************************************************************
!  CDROP_STACK
!
!  Drop a number from the complex stack.
!***********************************************************************************************************************************

SUBROUTINE CDROP_STACK (N)

INTEGER, INTENT(IN) :: N

INTEGER :: I


DO I = N, STACK_SIZE-1
   CSTACK(I) = CSTACK(I+1)
END DO

END SUBROUTINE CDROP_STACK



!***********************************************************************************************************************************
!  RDROP_STACK
!
!  Drop a number from the rational stack.
!***********************************************************************************************************************************

SUBROUTINE RDROP_STACK (N)

INTEGER, INTENT(IN) :: N

INTEGER :: I


DO I = N, STACK_SIZE-1
   RNSTACK(I) = RNSTACK(I+1)
   RDSTACK(I) = RDSTACK(I+1)
END DO

END SUBROUTINE RDROP_STACK


!***********************************************************************************************************************************
!  PRINTX
!
!  Print a real number to a string.
!***********************************************************************************************************************************

SUBROUTINE PRINTX (X, NUMSTR)

real(wp), INTENT(IN) :: X
CHARACTER(LEN=100), INTENT(OUT) :: NUMSTR

real(wp) :: TMPX
CHARACTER(LEN=100) :: FMTSTR

IF (BASE_MODE .EQ. 10) THEN                                                   ! DEC mode
   SELECT CASE (DISP_MODE)
      CASE (1)                                                                ! print X (FIX)
         WRITE (UNIT=FMTSTR, FMT='(1H(,4HF15.,I0,1H))') DISP_DIGITS
         WRITE (UNIT=NUMSTR, FMT=FMTSTR) X
         IF (INDEX(NUMSTR,'*') .NE. 0) THEN                                   !   disp. overflow
            WRITE (UNIT=FMTSTR, FMT='(1H(,5HES15.,I0,1H))') DISP_DIGITS
            WRITE (UNIT=NUMSTR, FMT=FMTSTR) X
         END IF
         READ (UNIT=NUMSTR, FMT=*) TMPX
         IF ((X .NE. 0._wp) .AND. (TMPX .EQ. 0._wp)) THEN                     !   disp. underflow
            WRITE (UNIT=FMTSTR, FMT='(1H(,5HES15.,I0,1H))') DISP_DIGITS
            WRITE (UNIT=NUMSTR, FMT=FMTSTR) X
         END IF
      CASE (2)                                                                ! print X (SCI)
         WRITE (UNIT=FMTSTR, FMT='(1H(,5HES15.,I0,1H))') DISP_DIGITS
         WRITE (UNIT=NUMSTR, FMT=FMTSTR) X
      CASE (3)                                                                ! print X (ENG)
         WRITE (UNIT=FMTSTR, FMT='(1H(,5HEN15.,I0,1H))') DISP_DIGITS
         WRITE (UNIT=NUMSTR, FMT=FMTSTR) X
      CASE (4)                                                                ! print X (ALL)
         WRITE (UNIT=FMTSTR, FMT='(A)') '(1PG23.15)'
         WRITE (UNIT=NUMSTR, FMT=FMTSTR) X
   END SELECT
ELSE
   SELECT CASE (BASE_MODE)
      CASE (2)                                                                ! print X (BIN)
         WRITE (UNIT=FMTSTR, FMT='(A)') '(B0)'
         WRITE (UNIT=NUMSTR, FMT=FMTSTR) INT(X)
      CASE (8)                                                                ! print X (OCT)
         WRITE (UNIT=FMTSTR, FMT='(A)') '(O0)'
         WRITE (UNIT=NUMSTR, FMT=FMTSTR) INT(X)
      CASE (16)                                                               ! print X (HEX)
         WRITE (UNIT=FMTSTR, FMT='(A)') '(Z0)'
         WRITE (UNIT=NUMSTR, FMT=FMTSTR) INT(X)
   END SELECT
END IF

END SUBROUTINE PRINTX



!***********************************************************************************************************************************
!  CPRINTX
!
!  Print a complex number to a string.
!***********************************************************************************************************************************

      SUBROUTINE CPRINTX (X, NUMSTR)

      COMPLEX(wp), INTENT(IN) :: X
      CHARACTER(LEN=100), INTENT(OUT) :: NUMSTR

      COMPLEX(wp) :: TMPX
      CHARACTER(LEN=100) :: FMTSTR

      IF (BASE_MODE .EQ. 10) THEN                                                   ! DEC mode
         SELECT CASE (DISP_MODE)
            CASE (1)                                                                ! print X (FIX)
               WRITE (UNIT=FMTSTR, FMT=800) DISP_DIGITS, DISP_DIGITS
  800          FORMAT ("(ES25.",I0,",SP,4X,F25.",I0,",2H i)")
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
               IF (INDEX(NUMSTR,'*') .NE. 0) THEN                                   !   disp. overflow
                  WRITE (UNIT=FMTSTR, FMT=810)  DISP_DIGITS, DISP_DIGITS
  810             FORMAT ("(EN25.",I0,",SP,4X,ES25.",I0,",2H i)")
                  WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
               END IF
               READ (UNIT=NUMSTR, FMT=*) TMPX
               IF ((X .NE. 0._wp) .AND. (TMPX .EQ. 0._wp)) THEN                     !   disp. underflow
                  WRITE (UNIT=FMTSTR, FMT=820) DISP_DIGITS, DISP_DIGITS
  820             FORMAT ("(EN25.",I0,",SP,4X,ES25.",I0,",2H i)")
                  WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
               END IF
            CASE (2)                                                                ! print X (SCI)
               WRITE (UNIT=FMTSTR, FMT=830) DISP_DIGITS, DISP_DIGITS
  830          FORMAT ("(ES25.",I0,",SP,4X,ES25.",I0,",2H i)")
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
            CASE (3)                                                                ! print X (ENG)
               WRITE (UNIT=FMTSTR, FMT=840) DISP_DIGITS, DISP_DIGITS
  840          FORMAT ("(EN25.",I0,",SP,4X,ES25.",I0,",2H i)")
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
            CASE (4)                                                                ! print X (ALL)
               WRITE (UNIT=FMTSTR, FMT='(A)') '(1PG23.15,SP,4X,G23.15,2H i)'
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
         END SELECT
      ELSE
         SELECT CASE (BASE_MODE)
            CASE (2)                                                                ! print X (BIN)
               WRITE (UNIT=FMTSTR, FMT='(A)') '(B0,4X,B0,2H i)'
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) INT(REAL(X, WP)), INT(AIMAG(X))
            CASE (8)                                                                ! print X (OCT)
               WRITE (UNIT=FMTSTR, FMT='(A)') '(O0,4X,O0,2H i)'
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) INT(REAL(X, WP)), INT(AIMAG(X))
            CASE (16)                                                               ! print X (HEX)
               WRITE (UNIT=FMTSTR, FMT='(A)') '(Z0,4X,Z0,2H i)'
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) INT(REAL(X, WP)), INT(AIMAG(X))
         END SELECT
      END IF

      RETURN

      END SUBROUTINE CPRINTX





!***********************************************************************************************************************************
!  RPRINTX
!
!  Print a rational number to a string.
!***********************************************************************************************************************************

SUBROUTINE RPRINTX (RN, RD, NUMSTR)

INTEGER, INTENT(IN) :: RN, RD
CHARACTER(LEN=100), INTENT(OUT) :: NUMSTR
INTEGER :: A1, A2, A3


SELECT CASE (BASE_MODE)
   CASE (2)                                                                   ! print X (BIN)
      IF (RD .EQ. 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(B0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(B0,3H / ,B0)') RN, RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(B0,3X,B0,3H / ,B0)') A1, A2, A3
         END SELECT
      END IF
   CASE (8)                                                                   ! print X (OCT)
      IF (RD .EQ. 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(O0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(O0,3H / ,O0)') RN, RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(O0,3X,O0,3H / ,O0)') A1, A2, A3
         END SELECT
      END IF
   CASE (10)                                                                  ! print X (DEC)
      IF (RD .EQ. 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(I0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(I0,3H / ,I0)') RN, RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(I0,3X,I0,3H / ,I0)') A1, A2, A3
         END SELECT
      END IF
   CASE (16)                                                                  ! print X (HEX)
      IF (RD .EQ. 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(Z0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(Z0,3H / ,Z0)') RN, RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(Z0,3X,Z0,3H / ,Z0)') A1, A2, A3
         END SELECT
      END IF
!         CASE (16)                                                                  ! print X (HEX)
!            IF (RD .EQ. 1) THEN
!               WRITE (UNIT=NUMSTR, FMT='(Z0)') RN
!            ELSE
!               WRITE (UNIT=NUMSTR, FMT='(Z0,3H / ,Z0)') RN, RD
!            END IF
END SELECT

RETURN

END SUBROUTINE RPRINTX





!***********************************************************************************************************************************
!  EVAL
!
!  Evaluate a operation.
!***********************************************************************************************************************************

SUBROUTINE EVAL (STR)

CHARACTER(LEN=*), INTENT(IN) :: STR

real(wp), PARAMETER :: PI = 4._wp * atan(1._wp)
real(wp), PARAMETER :: TWOPI = 2*pi
real(wp), PARAMETER :: LN2 = 0.6931471805599453094172321214581765680755001343602552541206800094933936219696947156059D0
real(wp), PARAMETER :: EULER = 0.57721566490153286060651209008240243104215933593992359880576723488486772677766467094D0
real(wp), PARAMETER :: GOLDEN = 1.6180339887498948482045868343656381177203091798057628621354486227052604628189024497D0
COMPLEX(wp),  PARAMETER :: II = (0._wp,1._wp)
real(wp), PARAMETER :: KG_PER_LB = 0.45359237D0
real(wp), PARAMETER :: CM_PER_IN = 2.54D0
real(wp), PARAMETER :: L_PER_GAL = 3.785411784D0
real(wp), PARAMETER :: A0 = 0.5291772108D-10                          ! m
real(wp), PARAMETER :: AMU = 1.66053886D-27                           ! kg
real(wp), PARAMETER :: AU = 1.49597870D11                             ! m
real(wp), PARAMETER :: C = 299792458.0D0                              ! m/s
real(wp), PARAMETER :: ECHG = 1.60217653D-19                          ! C
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

INTEGER :: I, J, ITMP, ITMP2, IERR, NUM, DEN, NUM2, DEN2, NUM3, DEN3, NUM4, DEN4, &
   NUMM, DENM, NUMB, DENB
real(wp) :: TMP, TMP2, TMP3, TMPM, TMPB, TMPR, BES_X, BES_ALPHA
COMPLEX(wp) :: CTMP, CTMP2, CTMP3, CTMPM, CTMPB, CTMPR
CHARACTER(LEN=2) :: REGNAME
INTEGER :: DT(8)
INTEGER :: YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, BES_NB, BES_NCALC
CHARACTER(LEN=100) :: FMTSTR, NUMSTR
CHARACTER(LEN=10) :: TIME, DATE, ZONE

real(wp), ALLOCATABLE, DIMENSION(:) :: BES_B

IF (LEN_TRIM(STR) .EQ. 0) THEN
   CONTINUE

ELSE IF (TRIM(STR) .EQ. '+') THEN                                             ! +
   SELECT CASE (DOMAIN_MODE)
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

ELSE IF (TRIM(STR) .EQ. '-') THEN                                             ! -
   SELECT CASE (DOMAIN_MODE)
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

ELSE IF (TRIM(STR) .EQ. '*') THEN                                             ! *
   SELECT CASE (DOMAIN_MODE)
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

ELSE IF (TRIM(STR) .EQ. '/') THEN                                             ! /
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  Divide Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = STACK(2) / STACK(1)
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0.0,0.0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  Divide Error'
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

ELSE IF (TRIM(STR) .EQ. '^') THEN                                             ! ^
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(2) ** STACK(1)
         CALL DROP_STACK(2)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(2) ** CSTACK(1)
         CALL CDROP_STACK(2)
      CASE (3)
         IF (RDSTACK(1) .EQ. 1) THEN
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

ELSE IF (TRIM(STR) .EQ. '\') THEN                                             ! \
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  Divide Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = 1._wp / STACK(1)
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0.0,0.0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  Divide Error'
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

ELSE IF (TRIM(STR) .EQ. '%') THEN                                             ! %
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = STACK(2) * 1.0D-2*STACK(1)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CSTACK(2) * 1.0D-2*CSTACK(1)
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

ELSE IF (TRIM(STR) .EQ. '%CHG') THEN                                          ! %CHG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(2) .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  Divide Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = 100.0D0*(STACK(1)-STACK(2))/STACK(2)
         END IF
      CASE (2)
         IF (CSTACK(2) .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  Divide Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = 100.0D0*(CSTACK(1)-CSTACK(2))/CSTACK(2)
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

ELSE IF (TRIM(STR) .EQ. '!') THEN                                             ! !
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISINT(STACK(1)).AND.(STACK(1).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  Factorial Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = gamma(STACK(1)+1._wp)
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (-1._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  Factorial Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CGAMMA(CSTACK(1)+(1._wp, 0._wp))
         END IF
      CASE (3)
         IF ((RDSTACK(1).EQ.1).AND.(RNSTACK(1).LT.0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  Factorial Error'
         ELSE
            IF (RDSTACK(1).EQ.1) THEN
               ITMP = RNSTACK(1)
               IF (ITMP.LT.0) THEN
                  WRITE (UNIT=*, FMT='(A)') '  Factorial Error'
               ELSE
                  ITMP2 = 1
                  DO I = 2, ITMP
                     ITMP2 = ITMP2 * I
                  END DO
                  RNLASTX = RNSTACK(1)
                  RDLASTX = RDSTACK(1)
                  RNSTACK(1) = ITMP2
                  RDSTACK(1) = 1
               END IF
            ELSE
               CALL SWITCH_RAT_TO_REAL
               IF (ISINT(STACK(1)).AND.(STACK(1).LT.0.0D0)) THEN
                  WRITE (UNIT=*, FMT='(A)') '  Factorial Error'
               ELSE
                  LASTX = STACK(1)
                  STACK(1) = gamma(STACK(1)+1._wp)
               END IF
            END IF
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. '!!') THEN                                            ! !!
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LT. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  !! Error'
         ELSE IF (ISFRAC(STACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  !! Error'
         ELSE IF (NINT(STACK(1)) .EQ. 0._wp) THEN
            LASTX = STACK(1)
            STACK(1) = 1._wp
         ELSE
            LASTX = STACK(1)
            ITMP = NINT(STACK(1))
            STACK(1) = 1._wp
            DO
               STACK(1) = STACK(1) * ITMP
               ITMP = ITMP - 2
               IF (ITMP .LE. 1) EXIT
            END DO
         END IF
      CASE (2)
         IF (real(CSTACK(1), wp) .LT. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  !! Error'
         ELSE IF (AIMAG(CSTACK(1)) .NE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  !! Error'
         ELSE IF (ISFRAC(real(CSTACK(1), wp))) THEN
            WRITE (UNIT=*, FMT='(A)') '  !! Error'
         ELSE IF (NINT(real(CSTACK(1), wp)) .EQ. 0._wp) THEN
            CLASTX = CSTACK(1)
            CSTACK(1) = (1._wp, 0._wp)
         ELSE
            CLASTX = CSTACK(1)
            ITMP = NINT(real(CSTACK(1), wp))
            TMP = 1._wp
            DO
               TMP = TMP * ITMP
               ITMP = ITMP - 2
               IF (ITMP .LE. 1) EXIT
            END DO
            CSTACK(1) = CMPLX(TMP, 0._wp, wp)
         END IF
      CASE (3)
         IF (RNSTACK(1) .LT. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  !! Error'
         ELSE IF (RDSTACK(1) .NE. 1) THEN
            WRITE (UNIT=*, FMT='(A)') '  !! Error'
         ELSE IF (RNSTACK(1) .EQ. 0) THEN
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
               IF (ITMP .LE. 1) EXIT
            END DO
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. '10X') THEN                                           ! 10X
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = 10.0D0**(STACK(1))
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = 10.0D0**(CSTACK(1))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = 10.0D0**(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. '2PI') THEN                                           ! 2PI
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (TWOPI)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(TWOPI, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (TWOPI)
   END SELECT

ELSE IF (TRIM(STR) .EQ. '2PII') THEN                                          ! 2PII
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='(A)') ' 2PIi not available in REAL mode'
      CASE (2)
         CALL CPUSH_STACK (CMPLX(0._wp,TWOPI, wp))
      CASE (3)
         WRITE (UNIT=*, FMT='(A)') ' 2PIi not available in RATIONAL mode'
   END SELECT

ELSE IF (TRIM(STR) .EQ. '2X') THEN                                            ! 2X
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

ELSE IF (TRIM(STR) .EQ. 'A0') THEN                                            ! A0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(A0, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (A0)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ABS') THEN                                           ! ABS
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

ELSE IF (TRIM(STR) .EQ. 'ACOS') THEN                                          ! ACOS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) .GT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOS Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOS(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = acos(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .GT. ABS(RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOS(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ACOSH') THEN                                         ! ACOSH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOSH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOSH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ACOSH(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) .LT. RDSTACK(1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOSH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOSH(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ACOT') THEN                                          ! ACOT
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

ELSE IF (TRIM(STR) .EQ. 'ACOT2') THEN                                         ! ACOT2
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

ELSE IF (TRIM(STR) .EQ. 'ACOTH') THEN                                         ! ACOTH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOTH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOTH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACOTH(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) .EQ. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOTH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOTH(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ACOVERS') THEN                                       ! ACOVERS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(1._wp-STACK(1)) .GT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOVERS Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACOVERS(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACOVERS(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .LT. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACOVERS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACOVERS(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ACRD') THEN                                          ! ACRD
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) .GT. 2.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACRD Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACRD(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACRD(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .GT. 2*ABS(RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACRD Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACRD(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ACSC') THEN                                          ! ACSC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) .LT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACSC Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACSC(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ACSC(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .LT. ABS(RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACSC Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACSC(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ACSCH') THEN                                         ! ACSCH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACSCH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ACSCH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CACSCH(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) .EQ. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  ACSCH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ACSCH(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'AEXSEC') THEN                                        ! AEXSEC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)+1._wp) .LT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  AEXSEC Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AEXSEC(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CAEXSEC(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .LT. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  AEXSEC Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = AEXSEC(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'AHAV') THEN                                          ! AHAV
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1).LT.0.0D0).OR.(STACK(1).GT.1._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  AHAV Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AHAV(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = AHAV(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF ((RNSTACK(1).LT.0).OR.(RNSTACK(1).GT.RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  AHAV Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = AHAV(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (STR(1:3) .EQ. 'ALL') THEN                                            ! ALL
   DISP_MODE = 4

ELSE IF (TRIM(STR) .EQ. 'AMU') THEN                                           ! AMU
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (AMU)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(AMU, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (AMU)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'AND') THEN                                           ! AND
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

ELSE IF (TRIM(STR) .EQ. 'ARG') THEN                                           ! ARG
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

ELSE IF (TRIM(STR) .EQ. 'ASEC') THEN                                          ! ASEC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) .LT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ASEC Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ASEC(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ASEC(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .LT. ABS(RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  ASEC Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            STACK(1) = ASEC(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ASECH') THEN                                         ! ASECH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1).LE.0.0D0).OR.(STACK(1).GT.1._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  ASECH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ASECH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CASECH(CSTACK(1))
      CASE (3)
         IF ((RNSTACK(1).LE.0).OR.(RNSTACK(1).GT.RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  ASECH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ASECH(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ASIN') THEN                                          ! ASIN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) .GT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ASIN Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ASIN(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ASIN(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .GT. ABS(RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  ASIN Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ASIN(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ASINH') THEN                                         ! ASINH
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

ELSE IF (TRIM(STR) .EQ. 'ATAN') THEN                                          ! ATAN
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

ELSE IF (TRIM(STR) .EQ. 'ATAN2') THEN                                         ! ATAN2
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

ELSE IF (TRIM(STR) .EQ. 'ATANH') THEN                                         ! ATANH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(STACK(1)) .GE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  ATANH Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = ATANH(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = ATANH(CSTACK(1))
      CASE (3)
         IF (ABS(RNSTACK(1)) .GE. ABS(RDSTACK(1))) THEN
            WRITE (UNIT=*, FMT='(A)') '  ATANH Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = ATANH(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'AU') THEN                                            ! AU
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (AU)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(AU, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (AU)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'AVERS') THEN                                         ! AVERS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ABS(1._wp-STACK(1)) .GT. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  AVERS Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AVERS(STACK(1))/ANGLE_FACTOR
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CAVERS(CSTACK(1))/ANGLE_FACTOR
      CASE (3)
         IF (ABS(RNSTACK(1)) .LT. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  AVERS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            LASTX = STACK(1)
            STACK(1) = AVERS(STACK(1))/ANGLE_FACTOR
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELJ0') THEN                                      ! BESSELJ0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = bessel_J0(STACK(1))
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELJ0 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = bessel_j0(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELJ1') THEN                                      ! BESSELJ1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = bessel_j1(STACK(1))
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELJ1 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = bessel_j1(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELJ') THEN                                       ! BESSELJ
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELJ Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RJBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. 0) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELJ Error 2'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELJ Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELJ not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELJ Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RJBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. 0) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELJ Error 2'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELJ Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELY0') THEN                                      ! BESSELY0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELY0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y0(STACK(1))
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELY0 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELY0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y0(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELY1') THEN                                      ! BESSELY1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELY1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y1(STACK(1))
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELY1 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELY1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = bessel_y1(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELY') THEN                                       ! BESSELY
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELY Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RYBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. 0) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELY Error 2'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELY Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELY not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELY Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RYBESL(BES_X, BES_ALPHA, BES_NB, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. 0) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELY Error 2'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELY Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELI0') THEN                                      ! BESSELI0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = BESI0(STACK(1))
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELI0 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = BESI0(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELI1') THEN                                      ! BESSELI1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = BESI1(STACK(1))
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELI1 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = BESI1(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELI') THEN                                       ! BESSELI
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELI Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RIBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. 0) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELI Error 2'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELI Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELI not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELI Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RIBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. 0) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELI Error 2'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELI Error 3'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELK0') THEN                                      ! BESSELK0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELK0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK0(STACK(1))
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELK0 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELK0 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK0(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELK1') THEN                                      ! BESSELK1
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELK1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK1(STACK(1))
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELK1 not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELK1 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = BESK1(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BESSELK') THEN                                       ! BESSELK
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RKBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. -1) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 2'
            ELSE IF (BES_NCALC .EQ. -1) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 3'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 4'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Error:  BESSELK not available '// &
            'in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 1'
         ELSE
            BES_X = STACK(1)
            BES_NB = INT(STACK(2)) + 1
            BES_ALPHA = FRAC(STACK(2))
            ALLOCATE (BES_B(BES_NB))
            CALL RKBESL(BES_X, BES_ALPHA, BES_NB, 1, BES_B, BES_NCALC)
            IF (BES_NCALC .LT. -1) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 2'
            ELSE IF (BES_NCALC .EQ. -1) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 3'
            ELSE IF (BES_NCALC .NE. BES_NB) THEN
               WRITE (UNIT=*, FMT='(A)') '  BESSELK Error 4'
            ELSE
               LASTX = STACK(1)
               STACK(1) = BES_B(BES_NB)
               CALL DROP_STACK(2)
            END IF
            DEALLOCATE (BES_B)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BETA') THEN                                          ! BETA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((ISINT(STACK(1)).AND.(STACK(1).LE.0.0D0)) .OR.  &
             (ISINT(STACK(2)).AND.(STACK(2).LE.0.0D0))) THEN
            WRITE (UNIT=*, FMT='(A)') '  BETA Error'
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
         IF (((RDSTACK(1).EQ.1).AND.(RNSTACK(1).LE.0)) .OR. &
             ((RDSTACK(2).EQ.1).AND.(RNSTACK(2).LE.0))) THEN
            WRITE (UNIT=*, FMT='(A)') '  BETA Error'
         ELSE
            IF ((RDSTACK(1).EQ.1) .AND. (RDSTACK(2).EQ.1)) THEN
               ITMP = RNSTACK(1)
               IF (ITMP.LE.0) THEN
                  WRITE (UNIT=*, FMT='(A)') '  BETA Error'
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
               IF ((ISINT(STACK(1)).AND.(STACK(1).LE.0.0D0)) .OR.  &
                   (ISINT(STACK(2)).AND.(STACK(2).LE.0.0D0))) THEN
                  WRITE (UNIT=*, FMT='(A)') '  BETA Error'
               ELSE
                  LASTX = STACK(1)
                  STACK(1) = BETA(STACK(1),STACK(2))
                  CALL DROP_STACK(2)
               END IF
            END IF
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'BIN') THEN                                           ! BIN
   BASE_MODE = 2

ELSE IF (TRIM(STR) .EQ. 'C') THEN                                             ! C
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (C)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(C, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (C)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'C>F') THEN                                           ! C>F
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

ELSE IF (TRIM(STR) .EQ. 'CBRT') THEN                                          ! CBRT
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

ELSE IF (TRIM(STR) .EQ. 'CHS') THEN                                           ! CHS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK(1) = -STACK(1)
      CASE (2)
         CSTACK(1) = -CSTACK(1)
      CASE (3)
         RNSTACK(1) = -RNSTACK(1)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'CLALL') THEN                                         ! CLALL
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

ELSE IF (TRIM(STR) .EQ. 'CLREG') THEN                                         ! CLREG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         REG = 0._wp
      CASE (2)
         CREG = (0._wp, 0._wp)
      CASE (3)
         RNREG = 0; RDREG = 1
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'CLS') THEN                                           ! CLS
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

ELSE IF (TRIM(STR) .EQ. 'CLSTK') THEN                                         ! CLSTK
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK = 0._wp
      CASE (2)
         CSTACK = (0._wp, 0._wp)
      CASE (3)
         RNSTACK = 0; RDSTACK = 1
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'CLX') THEN                                           ! CLX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK(1) = 0._wp
      CASE (2)
         CSTACK(1) = (0._wp, 0._wp)
      CASE (3)
         RNSTACK(1) = 0; RDSTACK(1) = 1
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'CM>IN') THEN                                         ! CM>IN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         STACK(1) = STACK(1) / CM_PER_IN
      CASE (2)
         CSTACK(1) = CSTACK(1) / CM_PER_IN
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         STACK(1) = STACK(1) / CM_PER_IN
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'CNR') THEN                                           ! CNR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISFRAC(STACK(1)) .OR. ISFRAC(STACK(2))) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF (STACK(2) .LT. STACK(1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE
            ITMP  = NINT(STACK(1))
            ITMP2 = NINT(STACK(2))
            LASTX = STACK(1)
            STACK(1) = CNR (ITMP2, ITMP)
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)) .OR. ISFRAC(real(CSTACK(2), wp))) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF (real(CSTACK(1), wp).LT.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF (real(CSTACK(2), wp).LT.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF (AIMAG(CSTACK(1)).NE.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF (AIMAG(CSTACK(2)).NE.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF (real(CSTACK(2), wp) .LT. real(CSTACK(1), wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE
            ITMP  = NINT(real(CSTACK(1), wp))
            ITMP2 = NINT(real(CSTACK(2), wp))
            TMP = CNR (ITMP2, ITMP)
            CLASTX = CSTACK(1)
            CSTACK(1) = CMPLX(TMP, 0._wp, wp)
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF ((RDSTACK(1).NE.1).OR.(RDSTACK(2).NE.1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF ((RNSTACK(1).LT.0) .OR. (RNSTACK(2).LT.0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE IF (RNSTACK(2) .LT. RNSTACK(1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  CNR Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = CNR (RNSTACK(2), RNSTACK(1))
            CALL RDROP_STACK(2)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'COMPLEX') THEN                                       ! COMPLEX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         DOMAIN_MODE = 2
         DO I = 1, STACK_SIZE
            CSTACK(I) = CMPLX(STACK(I), 0._wp, wp)
         END DO
         DO I = 0, REG_SIZE-1
            CREG(I) = CMPLX(REG(I), 0._wp, wp)
         END DO
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

ELSE IF (TRIM(STR) .EQ. 'CONJ') THEN                                          ! CONJ
   SELECT CASE (DOMAIN_MODE)
      CASE (2)
         CSTACK(1) = CONJG(CSTACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'COS') THEN                                           ! COS
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

ELSE IF (TRIM(STR) .EQ. 'COSH') THEN                                          ! COSH
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

ELSE IF (TRIM(STR) .EQ. 'COT') THEN                                           ! COT
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

ELSE IF (TRIM(STR) .EQ. 'COTH') THEN                                          ! COTH
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

ELSE IF (TRIM(STR) .EQ. 'COVERS') THEN                                        ! COVERS
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

ELSE IF (TRIM(STR) .EQ. 'CRD') THEN                                           ! CRD
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

ELSE IF (TRIM(STR) .EQ. 'CSC') THEN                                           ! CSC
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

ELSE IF (TRIM(STR) .EQ. 'CSCH') THEN                                          ! CSCH
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

ELSE IF (TRIM(STR) .EQ. 'CUBE') THEN                                          ! CUBE
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

ELSE IF (TRIM(STR) .EQ. 'D>F') THEN                                           ! D>F
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
         CALL CPUSH_STACK(CMPLX(real(NUM, wp),real(NUM2, wp), wp))
         CALL CPUSH_STACK(CMPLX(real(DEN, wp),real(DEN2, wp), wp))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'D>R') THEN                                           ! D>R
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

ELSE IF (TRIM(STR) .EQ. 'DEC') THEN                                           ! DEC
   BASE_MODE = 10

ELSE IF (TRIM(STR) .EQ. 'DEFAULT') THEN                                       ! DEFAULT
   ANGLE_MODE = INITIAL_ANGLE_MODE

   SELECT CASE (ANGLE_MODE)
      CASE (1)
         ANGLE_FACTOR = PI/180.0D0
      CASE (2)
         ANGLE_FACTOR = 1._wp
      CASE (3)
         ANGLE_FACTOR = PI/200.0D0
      CASE (4)
         ANGLE_FACTOR = TWOPI
   END SELECT

   DISP_MODE = INITIAL_DISP_MODE
   DISP_DIGITS = INITIAL_DISP_DIGITS
   DOMAIN_MODE = INITIAL_DOMAIN_MODE
   BASE_MODE = INITIAL_BASE_MODE
   FRACTION_MODE = INITIAL_FRACTION_MODE

ELSE IF (TRIM(STR) .EQ. 'DEG') THEN                                           ! DEG
   ANGLE_MODE = 1
   ANGLE_FACTOR = PI/180.0D0

ELSE IF (TRIM(STR) .EQ. 'DIGAMMA') THEN                                       ! DIGAMMA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = PSI(STACK(1))
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') ' DIGAMMA function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = PSI(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'DUP') THEN                                           ! DUP
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK(STACK(1))
      CASE (2)
         CALL CPUSH_STACK(CSTACK(1))
      CASE (3)
         CALL RPUSH_STACK(RNSTACK(1),RDSTACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ECHG') THEN                                          ! ECHG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (ECHG)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(ECHG, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (ECHG)
   END SELECT

ELSE IF (STR(1:3) .EQ. 'ENG') THEN                                            ! ENG
   IF (LEN_TRIM(STR) .EQ. 3) THEN
      WRITE (UNIT=*, FMT='(A)') '  ENG Error'
   ELSE
      READ (UNIT=STR(4:), FMT=*, IOSTAT=IERR) ITMP
      IF (IERR .NE. 0) THEN
         WRITE (UNIT=*, FMT='(A)') '  ENG Error'
      ELSE
         DISP_MODE = 3
         DISP_DIGITS = ITMP
      END IF
   END IF

ELSE IF (TRIM(STR) .EQ. 'EPS0') THEN                                          ! EPS0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (EPS0)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(EPS0, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (EPS0)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ERF') THEN                                           ! ERF
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = erf(STACK(1))
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') ' ERF function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = erf(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ERFC') THEN                                          ! ERFC
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = erfc(STACK(1))
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') ' ERFC function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = erfc(STACK(1))
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'EULER') THEN                                         ! EULER
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (EULER)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(EULER, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (EULER)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'EXP') THEN                                           ! EXP
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

ELSE IF (TRIM(STR) .EQ. 'EXSEC') THEN                                         ! EXSEC
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

ELSE IF (TRIM(STR) .EQ. 'F>C') THEN                                           ! F>C
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

ELSE IF (STR(1:3) .EQ. 'FIX') THEN                                            ! FIX
   IF (LEN_TRIM(STR) .EQ. 3) THEN
      WRITE (UNIT=*, FMT='(A)') '  FIX Error'
   ELSE
      READ (UNIT=STR(4:), FMT=*, IOSTAT=IERR) ITMP
      IF (IERR .NE. 0) THEN
         WRITE (UNIT=*, FMT='(A)') '  FIX Error'
      ELSE
         DISP_MODE = 1
         DISP_DIGITS = ITMP
      END IF
   END IF

ELSE IF (TRIM(STR) .EQ. 'FRAC') THEN                                          ! FRAC
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

ELSE IF (TRIM(STR) .EQ. 'FRACTOL') THEN                                       ! FRACTOL
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

ELSE IF (TRIM(STR) .EQ. 'G') THEN                                             ! G
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (G)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(G, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (G)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'GAL>L') THEN                                         ! GAL>L
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

ELSE IF (TRIM(STR) .EQ. 'GAMMA') THEN                                         ! GAMMA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISINT(STACK(1)).AND.(STACK(1).LE.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  GAMMA Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = gamma(STACK(1))
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  GAMMA Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CGAMMA(CSTACK(1))
         END IF
      CASE (3)
         IF ((RDSTACK(1).EQ.1).AND.(RNSTACK(1).LE.0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  GAMMA Error'
         ELSE
            IF (RDSTACK(1).EQ.1) THEN
               ITMP = RNSTACK(1)
               IF (ITMP.LE.0) THEN
                  WRITE (UNIT=*, FMT='(A)') '  GAMMA Error'
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
               IF (ISINT(STACK(1)).AND.(STACK(1).LE.0.0D0)) THEN
                  WRITE (UNIT=*, FMT='(A)') '  GAMMA Error'
               ELSE
                  LASTX = STACK(1)
                  STACK(1) = gamma(STACK(1))
               END IF
            END IF
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'GCD') THEN                                           ! GCD
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISFRAC(STACK(1)).OR.ISFRAC(STACK(2))) THEN
            WRITE (UNIT=*, FMT='(A)') '  GCD Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = GCD(NINT(STACK(2)),NINT(STACK(1)))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)).OR.ISFRAC(real(CSTACK(2), wp)).OR. &
            (AIMAG(CSTACK(2)).NE.0.0D0).OR.(AIMAG(CSTACK(2)).NE.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  GCD Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = GCD(NINT(real(CSTACK(2), wp)),NINT(real(CSTACK(1), wp)))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF ((RDSTACK(1).NE.1).OR.(RDSTACK(2).NE.1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  GCD Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(2)
            RNSTACK(1) = GCD(RNSTACK(2),RNSTACK(1))
            RDSTACK(1) = 1
            CALL RDROP_STACK(2)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'GOLDEN') THEN                                        ! GOLDEN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (GOLDEN)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(GOLDEN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (GOLDEN)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'GRAD') THEN                                          ! GRAD
   ANGLE_MODE = 3
   ANGLE_FACTOR = PI/200.0D0

ELSE IF (TRIM(STR) .EQ. 'GRAV') THEN                                          ! GRAV
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (GRAV)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(GRAV, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (GRAV)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'H') THEN                                             ! H
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (H)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(H, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (H)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'H>HMS') THEN                                         ! H>HMS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL H2HMSD (STACK(1), ITMP, ITMP2, TMP)
         LASTX = STACK(1)
         STACK(1) = real(itmp, wp) + 1.0D-2*ITMP2 + 1.0D-4*TMP
      CASE (2)
         IF (AIMAG(CSTACK(1)) .NE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  H>HMS Error'
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

ELSE IF (TRIM(STR) .EQ. 'HBAR') THEN                                          ! HBAR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (HBAR)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(HBAR, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (HBAR)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'HEX') THEN                                           ! HEX
   BASE_MODE = 16

ELSE IF (TRIM(STR) .EQ. 'HMS>H') THEN                                         ! HMS>H
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         ITMP = INT(STACK(1))
         ITMP2 = INT(FRAC(STACK(1))*1.0D2)
         TMP = (STACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
         CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
         LASTX = STACK(1)
         STACK(1) = TMP2
      CASE (2)
         IF (AIMAG(CSTACK(1)) .NE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  HMS>H Error'
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

ELSE IF (TRIM(STR) .EQ. 'HMS+') THEN                                          ! HMS+
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
         IF (AIMAG(CSTACK(1)) .NE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  HMS+ Error'
         ELSE
            ITMP = INT(CSTACK(1))
            ITMP2 = INT(FRAC(CSTACK(1))*1.0e2_wp)
            TMP = (CSTACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
            CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
            ITMP = INT(CSTACK(2))
            ITMP2 = INT(FRAC(CSTACK(2))*1.0e2_wp)
            TMP = (CSTACK(2) - ITMP - ITMP2*1.0D-2)*1.0D4
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

ELSE IF (TRIM(STR) .EQ. 'HMS-') THEN                                          ! HMS-
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
         IF (AIMAG(CSTACK(1)) .NE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  HMS- Error'
         ELSE
            ITMP = INT(CSTACK(1))
            ITMP2 = INT(FRAC(CSTACK(1))*1.0D2)
            TMP = (CSTACK(1) - ITMP - ITMP2*1.0D-2)*1.0D4
            CALL HMS2H (ITMP, ITMP2, TMP, TMP2)
            ITMP = INT(CSTACK(2))
            ITMP2 = INT(FRAC(CSTACK(2))*1.0D2)
            TMP = (CSTACK(2) - ITMP - ITMP2*1.0D-2)*1.0D4
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

ELSE IF (TRIM(STR) .EQ. 'HAV') THEN                                           ! HAV
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

ELSE IF (TRIM(STR) .EQ. 'HYPOT') THEN                                         ! HYPOT
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

ELSE IF (TRIM(STR) .EQ. 'HYPOT3') THEN                                        ! HYPOT3
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

ELSE IF (TRIM(STR) .EQ. 'I') THEN                                             ! I
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='(A)') ' i not available in REAL mode'
      CASE (2)
         CALL CPUSH_STACK (II)
      CASE (3)
         WRITE (UNIT=*, FMT='(A)') ' i not available in RATIONAL mode'
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'IM') THEN                                            ! IM
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

ELSE IF (TRIM(STR) .EQ. 'IMPROPER') THEN                                      ! IMPROPER
   FRACTION_MODE = 1

ELSE IF (TRIM(STR) .EQ. 'IN>CM') THEN                                         ! IN>CM
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

ELSE IF (TRIM(STR) .EQ. 'INT') THEN                                           ! INT
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

ELSE IF (TRIM(STR) .EQ. 'INT/') THEN                                          ! INT/
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  INT/ Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = AINT(STACK(2) / STACK(1))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  INT/ Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CINT(CSTACK(2) / CSTACK(1))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF (RNSTACK(1) .EQ. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  INT/ Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = RNSTACK(1) / RDSTACK(1)
            RDSTACK(1) = 1
            CALL RDROP_STACK(2)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'KB') THEN                                            ! KB
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (KB)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(KB, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (KB)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'KEPLER') THEN                                        ! KEPLER
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

ELSE IF (TRIM(STR) .EQ. 'KG>LB') THEN                                         ! KG>LB
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

ELSE IF (TRIM(STR) .EQ. 'L>GAL') THEN                                         ! L>GAL
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

ELSE IF (TRIM(STR) .EQ. 'LASTX') THEN                                         ! LASTX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (LASTX)
      CASE (2)
         CALL CPUSH_STACK (CLASTX)
      CASE (3)
         CALL RPUSH_STACK (RNLASTX, RDLASTX)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'LB>KG') THEN                                         ! LB>KG
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

ELSE IF (TRIM(STR) .EQ. 'LCM') THEN                                           ! LCM
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (ISFRAC(STACK(1)).OR.ISFRAC(STACK(2))) THEN
            WRITE (UNIT=*, FMT='(A)') '  LCM Error'
         ELSE IF ((STACK(1).EQ.0.0D0).AND.(STACK(2).EQ.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  LCM Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LCM(NINT(STACK(2)),NINT(STACK(1)))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)).OR.ISFRAC(real(CSTACK(2), wp)).OR. &
            (AIMAG(CSTACK(2)).NE.0.0D0).OR.(AIMAG(CSTACK(2)).NE.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  LCM Error'
         ELSE IF ((CSTACK(1).EQ.(0._wp, 0._wp)).AND. &
            (CSTACK(2).EQ.(0._wp, 0._wp))) THEN
            WRITE (UNIT=*, FMT='(A)') '  LCM Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = LCM(NINT(real(CSTACK(2), wp)),NINT(real(CSTACK(1), wp)))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF ((RDSTACK(1).NE.1).OR.(RDSTACK(2).NE.1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  LCM Error'
         ELSE IF ((RNSTACK(1).EQ.0).AND.(RNSTACK(2).EQ.0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  LCM Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(2)
            RNSTACK(1) = LCM(RNSTACK(2),RNSTACK(1))
            RDSTACK(1) = 1
            CALL RDROP_STACK(2)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'LN') THEN                                            ! LN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LN Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1))
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  LN Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = LOG(CSTACK(1))
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LN Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'LOG') THEN                                           ! LOG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LOG Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG10(STACK(1))
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  LOG Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CLOG10(CSTACK(1))
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LOG Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG10(STACK(1))
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'LOG2') THEN                                          ! LOG2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LOG2 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1))/LN2
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  LOG2 Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = LOG(CSTACK(1))/LN2
         END IF
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         IF (STACK(1) .LE. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LOG2 Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = LOG(STACK(1))/LN2
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'LR') THEN                                            ! LR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LR Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            CALL PUSH_STACK (TMPM)
            CALL PUSH_STACK (TMPB)
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  LR Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CALL CPUSH_STACK (CTMPM)
            CALL CPUSH_STACK (CTMPB)
         END IF
      CASE (3)
         IF (RNNN .LE. 1) THEN
            WRITE (UNIT=*, FMT='(A)') '  LR Error'
         ELSE
            CALL RLINREG (NUMM,DENM,NUMB,DENB,TMPR)
            CALL RPUSH_STACK (NUMM,DENM)
            CALL RPUSH_STACK (NUMB,DENB)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'ME') THEN                                            ! ME
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (ME)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(ME, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (ME)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'MIXED') THEN                                         ! MIXED
   FRACTION_MODE = 2

ELSE IF (TRIM(STR) .EQ. 'MN') THEN                                            ! MN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MN)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(MN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MN)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'MOD') THEN                                           ! MOD
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  MOD Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = MOD (STACK(2),STACK(1))
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (CSTACK(1) .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  MOD Error'
         ELSE
            CLASTX = CSTACK(1)
            CSTACK(1) = CMOD (CSTACK(2),CSTACK(1))
            CALL CDROP_STACK(2)
         END IF
      CASE (3)
         IF (RNSTACK(1) .EQ. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  MOD Error'
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

ELSE IF (TRIM(STR) .EQ. 'MODES') THEN                                         ! MODES
   WRITE (UNIT=*, FMT='()')
   SELECT CASE (ANGLE_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='(A)') '  Angles:     DEG'
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Angles:     RAD'
      CASE (3)
         WRITE (UNIT=*, FMT='(A)') '  Angles:     GRAD'
      CASE (4)
         WRITE (UNIT=*, FMT='(A)') '  Angles:     REV'
   END SELECT
   SELECT CASE (DISP_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='(A,I0)') '  Display:    FIX ', DISP_DIGITS
      CASE (2)
         WRITE (UNIT=*, FMT='(A,I0)') '  Display:    SCI ', DISP_DIGITS
      CASE (3)
         WRITE (UNIT=*, FMT='(A,I0)') '  Display:    ENG ', DISP_DIGITS
      CASE (4)
         WRITE (UNIT=*, FMT='(A)') '  Display:    ALL '
   END SELECT
   SELECT CASE (BASE_MODE)
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Base:       BIN'
      CASE (8)
         WRITE (UNIT=*, FMT='(A)') '  Base:       OCT'
      CASE (10)
         WRITE (UNIT=*, FMT='(A)') '  Base:       DEC'
      CASE (16)
         WRITE (UNIT=*, FMT='(A)') '  Base:       HEX'
   END SELECT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='(A)') '  Domain:     REAL'
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Domain:     COMPLEX'
      CASE (3)
         WRITE (UNIT=*, FMT='(A)') '  Domain:     RATIONAL'
   END SELECT
   SELECT CASE (FRACTION_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='(A)') '  Fractions:  IMPROPER'
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') '  Fractions:  MIXED'
   END SELECT
   WRITE (UNIT=*, FMT='(A)') ' '

ELSE IF (TRIM(STR) .EQ. 'MP') THEN                                            ! MP
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MP)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(MP, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MP)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'MU0') THEN                                           ! MU0
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MU0)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(MU0, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MU0)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'MUB') THEN                                           ! MUB
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MUB)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(MUB, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MUB)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'MUN') THEN                                           ! MUN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (MUN)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(MUN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (MUN)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'N') THEN                                             ! N
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (NN)
      CASE (2)
         CALL CPUSH_STACK (CNN)
      CASE (3)
         CALL RPUSH_STACK (RNNN, RDNN)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'NA') THEN                                            ! NA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (NA)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(NA, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (NA)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'NOT') THEN                                           ! NOT
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

ELSE IF (TRIM(STR) .EQ. 'OCT') THEN                                           ! OCT
   BASE_MODE = 8

ELSE IF (TRIM(STR) .EQ. 'OR') THEN                                            ! OR
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

ELSE IF (TRIM(STR) .EQ. 'P>R') THEN                                           ! P>R
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

ELSE IF (TRIM(STR) .EQ. 'PI') THEN                                            ! PI
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (PI)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(PI, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (PI)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'PNR') THEN                                           ! PNR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF ((RDSTACK(1).NE.1).OR.(RDSTACK(2).NE.1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF ((RNSTACK(1).LT.0) .OR. (RNSTACK(2).LT.0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF (RNSTACK(2) .LT. RNSTACK(1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = PNR (ITMP2, ITMP)
            CALL DROP_STACK(2)
         END IF
      CASE (2)
         IF (ISFRAC(real(CSTACK(1), wp)) .OR. ISFRAC(real(CSTACK(2), wp))) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF (real(CSTACK(1), wp).LT.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF (real(CSTACK(2), wp).LT.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF (AIMAG(CSTACK(1)).NE.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF (AIMAG(CSTACK(2)).NE.0.0D0) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF (real(CSTACK(2), wp) .LT. real(CSTACK(1), wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
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
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF ((STACK(1).LT.0.0D0) .OR. (STACK(2).LT.0.0D0)) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE IF (STACK(2) .LT. STACK(1)) THEN
            WRITE (UNIT=*, FMT='(A)') '  PNR Error'
         ELSE
            RNLASTX = RNSTACK(1)
            RDLASTX = RDSTACK(1)
            RNSTACK(1) = PNR (RNSTACK(2), RNSTACK(1))
            CALL RDROP_STACK(2)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'PR') THEN                                            ! PR
   WRITE (UNIT=*, FMT='()')
   DO I = 0, REG_SIZE-1
      SELECT CASE (DOMAIN_MODE)
         CASE (1)
            CALL PRINTX(REG(I), NUMSTR)
         CASE (2)
            CALL CPRINTX(CREG(I), NUMSTR)
         CASE (3)
            CALL RPRINTX(RNREG(I), RDREG(I), NUMSTR)
      END SELECT
      WRITE (UNIT=*, FMT='(1X,I3,A)') I, ':  '//TRIM(NUMSTR)
   END DO
   WRITE (UNIT=*, FMT='(A)') ' '

ELSE IF (TRIM(STR) .EQ. 'PS') THEN                                            ! PS
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
            CALL CPRINTX(CSTACK(I), NUMSTR)
         CASE (3)
            CALL RPRINTX(RNSTACK(I), RDSTACK(I), NUMSTR)
      END SELECT
      WRITE (UNIT=*, FMT='(2X,A)') REGNAME//':  '//TRIM(NUMSTR)
   END DO
   WRITE (UNIT=*, FMT='(A)') ' '

ELSE IF (TRIM(STR) .EQ. 'PSUMS') THEN                                         ! PSUMS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         WRITE (UNIT=*, FMT='()')
         CALL PRINTX(NN, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  N:   '//TRIM(NUMSTR)
         CALL PRINTX(SUMX, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  X:   '//TRIM(NUMSTR)
         CALL PRINTX(SUMX2, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  X2:  '//TRIM(NUMSTR)
         CALL PRINTX(SUMY, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  Y:   '//TRIM(NUMSTR)
         CALL PRINTX(SUMY2, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  Y2:  '//TRIM(NUMSTR)
         CALL PRINTX(SUMXY, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  XY:  '//TRIM(NUMSTR)
         WRITE (UNIT=*, FMT='(A)') ' '
      CASE (2)
         WRITE (UNIT=*, FMT='()')
         CALL CPRINTX(CNN, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  N:   '//TRIM(NUMSTR)
         CALL CPRINTX(CSUMX, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  X:   '//TRIM(NUMSTR)
         CALL CPRINTX(CSUMX2, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  X2:  '//TRIM(NUMSTR)
         CALL CPRINTX(CSUMY, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  Y:   '//TRIM(NUMSTR)
         CALL CPRINTX(CSUMY2, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  Y2:  '//TRIM(NUMSTR)
         CALL CPRINTX(CSUMXY, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  XY:  '//TRIM(NUMSTR)
         WRITE (UNIT=*, FMT='(A)') ' '
      CASE (3)
         WRITE (UNIT=*, FMT='()')
         CALL RPRINTX(RNNN, RDNN, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  N:   '//TRIM(NUMSTR)
         CALL RPRINTX(RNSUMX, RDSUMX, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  X:   '//TRIM(NUMSTR)
         CALL RPRINTX(RNSUMX2, RDSUMX2, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  X2:  '//TRIM(NUMSTR)
         CALL RPRINTX(RNSUMY, RDSUMY, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  Y:   '//TRIM(NUMSTR)
         CALL RPRINTX(RNSUMY2, RDSUMY2, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  Y2:  '//TRIM(NUMSTR)
         CALL RPRINTX(RNSUMXY, RDSUMXY, NUMSTR)
         WRITE (UNIT=*, FMT='(A)') '  XY:  '//TRIM(NUMSTR)
         WRITE (UNIT=*, FMT='(A)') ' '
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'R') THEN                                             ! R
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         TMP = STACK(1)
         DO I = 1, STACK_SIZE-1
            STACK(I) = STACK(I+1)
         END DO
         STACK(STACK_SIZE) = TMP
      CASE (2)
         CTMP = CSTACK(1)
         DO I = 1, STACK_SIZE-1
            CSTACK(I) = CSTACK(I+1)
         END DO
         CSTACK(STACK_SIZE) = CTMP
      CASE (3)
         ITMP = RNSTACK(1)
         ITMP2 = RDSTACK(1)
         DO I = 1, STACK_SIZE-1
            RNSTACK(I) = RNSTACK(I+1)
            RDSTACK(I) = RDSTACK(I+1)
         END DO
         RNSTACK(STACK_SIZE) = ITMP
         RDSTACK(STACK_SIZE) = ITMP2
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'R>D') THEN                                           ! R>D
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

ELSE IF (TRIM(STR) .EQ. 'R>P') THEN                                           ! R>P
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

ELSE IF (TRIM(STR) .EQ. 'RAD') THEN                                           ! RAD
   ANGLE_MODE = 2
   ANGLE_FACTOR = 1._wp

ELSE IF (TRIM(STR) .EQ. 'RAND') THEN                                          ! RAND
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL RANDOM_NUMBER (TMP)
         CALL PUSH_STACK(TMP)
      CASE (2)
         CALL RANDOM_NUMBER (TMP)
         CALL RANDOM_NUMBER (TMP2)
         CALL CPUSH_STACK(CMPLX(TMP,TMP2, wp))
      CASE (3)
         CALL RANDOM_NUMBER (TMP)
         CALL DEC_TO_FRAC (TMP, NUM, DEN, FRACTOL)
         CALL RPUSH_STACK(NUM, DEN)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'RATIONAL') THEN                                      ! RATIONAL
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

ELSE IF (STR(1:3) .EQ. 'RCL') THEN                                            ! RCL
   IF (LEN_TRIM(STR) .EQ. 3) THEN
      WRITE (UNIT=*, FMT='(A)') '  RCL Error'
   ELSE
      READ (UNIT=STR(4:), FMT=*, IOSTAT=IERR) ITMP
      IF (IERR .NE. 0) THEN
         WRITE (UNIT=*, FMT='(A)') '  RCL Error'
      ELSE
         IF ((ITMP.LT.0).OR.(ITMP.GE.REG_SIZE)) THEN
            WRITE (UNIT=*, FMT='(A)') '  RCL Error'
         ELSE
            SELECT CASE (DOMAIN_MODE)
               CASE (1)
                  CALL PUSH_STACK(REG(ITMP))
               CASE (2)
                  CALL CPUSH_STACK(CREG(ITMP))
               CASE (3)
                  CALL RPUSH_STACK(RNREG(ITMP),RDREG(ITMP))
            END SELECT
         END IF
      END IF
   END IF
   PRINT *, REG(ITMP)

ELSE IF (TRIM(STR) .EQ. 'RCORR') THEN                                         ! RCORR
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  RCORR Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            CALL PUSH_STACK (TMPR)
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  RCORR Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CALL CPUSH_STACK (CTMPR)
         END IF
      CASE (3)
         IF (RNNN .LE. 1) THEN
            WRITE (UNIT=*, FMT='(A)') '  RCORR Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            CALL LINREG (TMPM,TMPB,TMPR)
            CALL PUSH_STACK (TMPR)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'RE') THEN                                            ! RE
   SELECT CASE (DOMAIN_MODE)
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(real(CSTACK(1), wp), 0._wp, wp)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'REAL') THEN                                          ! REAL
   SELECT CASE (DOMAIN_MODE)
      CASE (2)
         DOMAIN_MODE = 1
         DO I = 1, STACK_SIZE
            STACK(I) = real(CSTACK(I), wp)
         END DO
         DO I = 0, REG_SIZE-1
            REG(I) = real(CREG(I), wp)
         END DO
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

ELSE IF (TRIM(STR) .EQ. 'REARTH') THEN                                        ! REARTH
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (REARTH)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(REARTH, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (REARTH)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'REDUCE') THEN                                        ! REDUCE
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

ELSE IF (TRIM(STR) .EQ. 'RESET') THEN                                         ! RESET
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
         ANGLE_FACTOR = TWOPI
   END SELECT

   DISP_MODE = INITIAL_DISP_MODE
   DISP_DIGITS = INITIAL_DISP_DIGITS
   DOMAIN_MODE = INITIAL_DOMAIN_MODE
   BASE_MODE = INITIAL_BASE_MODE
   FRACTION_MODE = INITIAL_FRACTION_MODE

   FRACTOL = INITIAL_FRACTOL

ELSE IF (TRIM(STR) .EQ. 'REV') THEN                                           ! REV
   ANGLE_MODE = 4
   ANGLE_FACTOR = TWOPI

ELSE IF (TRIM(STR) .EQ. 'RGAS') THEN                                          ! RGAS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (RGAS)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(RGAS, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (RGAS)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'RI') THEN                                            ! RI
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

ELSE IF (TRIM(STR) .EQ. 'ROUND') THEN                                         ! ROUND
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

ELSE IF (TRIM(STR) .EQ. 'RUP') THEN                                           ! RUP
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         TMP = STACK(STACK_SIZE)
         DO I = STACK_SIZE, 2, -1
            STACK(I) = STACK(I-1)
         END DO
         STACK(1) = TMP
      CASE (2)
         CTMP = CSTACK(STACK_SIZE)
         DO I = STACK_SIZE, 2, -1
            CSTACK(I) = CSTACK(I-1)
         END DO
         CSTACK(1) = CTMP
      CASE (3)
         ITMP = RNSTACK(STACK_SIZE)
         ITMP2 = RDSTACK(STACK_SIZE)
         DO I = STACK_SIZE, 2, -1
            RNSTACK(I) = RNSTACK(I-1)
            RDSTACK(I) = RDSTACK(I-1)
         END DO
         RNSTACK(1) = ITMP
         RDSTACK(1) = ITMP2
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'RZETA') THEN                                         ! RZETA
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         LASTX = STACK(1)
         STACK(1) = RIEMANNZETA(STACK(1), 1.e-10_wp) + 1._wp
      CASE (2)
         WRITE (UNIT=*, FMT='(A)') ' RZETA function not available in COMPLEX mode.'
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         LASTX = STACK(1)
         STACK(1) = RIEMANNZETA(STACK(1), 1.e-10_wp) + 1._wp
   END SELECT

ELSE IF (STR(1:3) .EQ. 'S') THEN                                              ! S
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

ELSE IF (STR(1:3) .EQ. 'S-') THEN                                             ! S-
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

ELSE IF (STR(1:3) .EQ. 'SCI') THEN                                            ! SCI
   IF (LEN_TRIM(STR) .EQ. 3) THEN
      WRITE (UNIT=*, FMT='(A)') '  SCI Error'
   ELSE
      READ (UNIT=STR(4:), FMT=*, IOSTAT=IERR) ITMP
      IF (IERR .NE. 0) THEN
         WRITE (UNIT=*, FMT='(A)') '  SCI Error'
      ELSE
         DISP_MODE = 2
         DISP_DIGITS = ITMP
      END IF
   END IF

ELSE IF (TRIM(STR) .EQ. 'SEC') THEN                                           ! SEC
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

ELSE IF (TRIM(STR) .EQ. 'SECH') THEN                                          ! SECH
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

ELSE IF (TRIM(STR) .EQ. 'SGN') THEN                                           ! SGN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LT. 0._wp) THEN
            TMP = -1._wp
         ELSE IF (STACK(1) .EQ. 0._wp) THEN
            TMP = 0._wp
         ELSE
            TMP = +1._wp
         END IF
         LASTX = STACK(1)
         STACK(1) = TMP
      CASE (2)
         IF (real(CSTACK(1), wp) .LT. 0._wp) THEN
            TMP = -1._wp
         ELSE IF (real(CSTACK(1), wp) .EQ. 0._wp) THEN
            TMP = 0._wp
         ELSE
            TMP = +1._wp
         END IF
         IF (AIMAG(CSTACK(1)) .LT. 0._wp) THEN
            TMP2 = -1._wp
         ELSE IF (AIMAG(CSTACK(1)) .EQ. 0._wp) THEN
            TMP2 = 0._wp
         ELSE
            TMP2 = +1._wp
         END IF
         CLASTX = CSTACK(1)
         CSTACK(1) = CMPLX(TMP,TMP2, wp)
      CASE (3)
         IF (RNSTACK(1) .LT. 0) THEN
            ITMP = 1
         ELSE IF (RNSTACK(1) .EQ. 0) THEN
            ITMP = 0
         ELSE
            ITMP = +1
         END IF
         RNLASTX = RNSTACK(1)
         RDLASTX = RDSTACK(1)
         RNSTACK(1) = ITMP
         RDSTACK(1) = 1
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'SIN') THEN                                           ! SIN
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

ELSE IF (TRIM(STR) .EQ. 'SINC') THEN                                          ! SINC
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

ELSE IF (TRIM(STR) .EQ. 'SINH') THEN                                          ! SINH
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

ELSE IF (TRIM(STR) .EQ. 'SINHC') THEN                                         ! SINHC
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

ELSE IF (TRIM(STR) .EQ. 'SQR') THEN                                           ! SQR
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

ELSE IF (TRIM(STR) .EQ. 'SQRT') THEN                                          ! SQRT
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (STACK(1) .LT. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  SQRT Error'
         ELSE
            LASTX = STACK(1)
            STACK(1) = SQRT(STACK(1))
         END IF
      CASE (2)
         CLASTX = CSTACK(1)
         CSTACK(1) = SQRT(CSTACK(1))
      CASE (3)
         IF (RNSTACK(1) .LT. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  SQRT Error'
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

ELSE IF (TRIM(STR) .EQ. 'STEFAN') THEN                                        ! STEFAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (STEFAN)
      CASE (2)
         CALL CPUSH_STACK (CMPLX(STEFAN, 0._wp, wp))
      CASE (3)
         CALL SWITCH_RAT_TO_REAL
         CALL PUSH_STACK (STEFAN)
   END SELECT

ELSE IF (STR(1:3) .EQ. 'STO') THEN                                            ! STO
   IF (LEN_TRIM(STR) .EQ. 3) THEN
      WRITE (UNIT=*, FMT='(A)') '  STO Error'
   ELSE
      READ (UNIT=STR(4:), FMT=*, IOSTAT=IERR) ITMP
      IF (IERR .NE. 0) THEN
         WRITE (UNIT=*, FMT='(A)') '  STO Error'
      ELSE
         IF ((ITMP.LT.0).OR.(ITMP.GE.REG_SIZE)) THEN
            WRITE (UNIT=*, FMT='(A)') '  STO Error'
         ELSE
            SELECT CASE (DOMAIN_MODE)
               CASE (1)
                  REG(ITMP) = STACK(1)
               CASE (2)
                  CREG(ITMP) = CSTACK(1)
               CASE (3)
                  RNREG(ITMP) = RNSTACK(1)
                  RDREG(ITMP) = RDSTACK(1)
            END SELECT
         END IF
      END IF
   END IF
   PRINT *, REG(ITMP)

ELSE IF (TRIM(STR) .EQ. 'SUMX') THEN                                          ! SUMX
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMX)
      CASE (2)
         CALL CPUSH_STACK (CSUMX)
      CASE (3)
         CALL RPUSH_STACK (RNSUMX,RDSUMX)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'SUMX2') THEN                                         ! SUMX2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMX2)
      CASE (2)
         CALL CPUSH_STACK (CSUMX2)
      CASE (3)
         CALL RPUSH_STACK (RNSUMX2,RDSUMX2)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'SUMXY') THEN                                         ! SUMXY
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMXY)
      CASE (2)
         CALL CPUSH_STACK (CSUMXY)
      CASE (3)
         CALL RPUSH_STACK (RNSUMXY,RDSUMXY)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'SUMY') THEN                                          ! SUMY
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMY)
      CASE (2)
         CALL CPUSH_STACK (CSUMY)
      CASE (3)
         CALL RPUSH_STACK (RNSUMY,RDSUMY)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'SUMY2') THEN                                         ! SUMY2
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         CALL PUSH_STACK (SUMY2)
      CASE (2)
         CALL CPUSH_STACK (CSUMY2)
      CASE (3)
         CALL RPUSH_STACK (RNSUMY2,RDSUMY2)
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'TAN') THEN                                           ! TAN
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

ELSE IF (TRIM(STR) .EQ. 'TANC') THEN                                          ! TANC
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

ELSE IF (TRIM(STR) .EQ. 'TANH') THEN                                          ! TANH
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

ELSE IF (TRIM(STR) .EQ. 'TANHC') THEN                                         ! TANHC
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

ELSE IF (TRIM(STR) .EQ. 'TIME') THEN                                          ! TIME
   CALL DATE_AND_TIME (DATE, TIME, ZONE, DT)
   YEAR = DT(1)
   MONTH = DT(2)
   DAY = DT(3)
   HOUR = DT(5)
   MINUTE = DT(6)
   SECOND = DT(7)
   WRITE (UNIT=*, FMT='(/A,I2,1H/,I2,1H/,I4)') '  Date:  ', MONTH, DAY, YEAR
   WRITE (UNIT=*, FMT='(A,I2.2,1H:,I2.2,1H:,I2.2/)') &
      '  Time:  ', HOUR, MINUTE, SECOND

ELSE IF (TRIM(STR) .EQ. 'VER') THEN                                           ! VER
   WRITE (UNIT=*, FMT='(/A/)') '  RPN Version '//VERSION

ELSE IF (TRIM(STR) .EQ. 'VERS') THEN                                          ! VERS
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

ELSE IF (TRIM(STR) .EQ. 'X^') THEN                                            ! X^
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  X^ Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            LASTX = STACK(1)
            STACK(1) = (STACK(1)-TMPB)/TMPM
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  X^ Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CLASTX = CSTACK(1)
            CSTACK(1) = (CSTACK(1)-CTMPB)/CTMPM
         END IF
      CASE (3)
         IF (RNNN .LE. 1) THEN
            WRITE (UNIT=*, FMT='(A)') '  X^ Error'
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

ELSE IF (TRIM(STR) .EQ. 'XMEAN') THEN                                         ! XMEAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  XMEAN Error'
         ELSE
            TMP = SUMX/NN
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (CNN .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  XMEAN Error'
         ELSE
            CTMP = CSUMX/CNN
            CALL CPUSH_STACK(CTMP)
         END IF
      CASE (3)
         IF (RNNN .EQ. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  XMEAN Error'
         ELSE
            CALL RDIV (RNSUMX,RDSUMX,RNNN,RDNN,NUM,DEN)
            CALL RPUSH_STACK(NUM,DEN)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'XOR') THEN                                           ! XOR
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

ELSE IF (TRIM(STR) .EQ. 'XRT') THEN                                           ! XRT
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

ELSE IF (TRIM(STR) .EQ. 'XS') THEN                                            ! XS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  XS Error'
         ELSE
            TMP = SQRT((SUMX2-SUMX**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  XS Error'
         ELSE
            CTMP = SQRT((CSUMX2-CSUMX**2/CNN)/(CNN-1._wp))
            CALL CPUSH_STACK(CTMP)
         END IF
      CASE (3)
         IF (RNNN .LE. RDNN) THEN
            WRITE (UNIT=*, FMT='(A)') '  XS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMX2-SUMX**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'XSIG') THEN                                          ! XSIG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  XSIG Error'
         ELSE
            TMP = SQRT((SUMX2-SUMX**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  XSIG Error'
         ELSE
            CTMP = SQRT((CSUMX2-CSUMX**2/CNN)/CNN)
            CALL CPUSH_STACK(CTMP)
         END IF
      CASE (3)
         IF (RNNN .LE. RDNN) THEN
            WRITE (UNIT=*, FMT='(A)') '  XSIG Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMX2-SUMX**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'XY') THEN                                            ! XY
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

ELSE IF (TRIM(STR) .EQ. 'Y^') THEN                                            ! Y^
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  Y^ Error'
         ELSE
            CALL LINREG (TMPM,TMPB,TMPR)
            LASTX = STACK(1)
            STACK(1) = TMPM*STACK(1)+TMPB
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  Y^ Error'
         ELSE
            CALL CLINREG (CTMPM,CTMPB,CTMPR)
            CLASTX = CSTACK(1)
            CSTACK(1) = CTMPM*CSTACK(1)+CTMPB
         END IF
      CASE (3)
         IF (RNNN .LE. 1) THEN
            WRITE (UNIT=*, FMT='(A)') '  Y^ Error'
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

ELSE IF (TRIM(STR) .EQ. 'YMEAN') THEN                                         ! YMEAN
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .EQ. 0._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  YMEAN Error'
         ELSE
            TMP = SUMY/NN
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (CNN .EQ. (0._wp, 0._wp)) THEN
            WRITE (UNIT=*, FMT='(A)') '  YMEAN Error'
         ELSE
            CTMP = CSUMY/CNN
            CALL CPUSH_STACK(CTMP)
         END IF
      CASE (3)
         IF (RNNN .EQ. 0) THEN
            WRITE (UNIT=*, FMT='(A)') '  YMEAN Error'
         ELSE
            CALL RDIV (RNSUMY,RDSUMY,RNNN,RDNN,NUM,DEN)
            CALL RPUSH_STACK(NUM,DEN)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'YS') THEN                                            ! YS
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  YS Error'
         ELSE
            TMP = SQRT((SUMY2-SUMY**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  YS Error'
         ELSE
            CTMP = SQRT((CSUMY2-CSUMY**2/CNN)/(CNN-1._wp))
            CALL CPUSH_STACK(CTMP)
         END IF
      CASE (3)
         IF (RNNN .LE. RDNN) THEN
            WRITE (UNIT=*, FMT='(A)') '  YS Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMY2-SUMY**2/NN)/(NN-1._wp))
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

ELSE IF (TRIM(STR) .EQ. 'YSIG') THEN                                          ! YSIG
   SELECT CASE (DOMAIN_MODE)
      CASE (1)
         IF (NN .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  YSIG Error'
         ELSE
            TMP = SQRT((SUMY2-SUMY**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
      CASE (2)
         IF (real(CNN, wp) .LE. 1._wp) THEN
            WRITE (UNIT=*, FMT='(A)') '  YSIG Error'
         ELSE
            CTMP = SQRT((CSUMY2-CSUMY**2/CNN)/CNN)
            CALL CPUSH_STACK(CTMP)
         END IF
      CASE (3)
         IF (RNNN .LE. RDNN) THEN
            WRITE (UNIT=*, FMT='(A)') '  YSIG Error'
         ELSE
            CALL SWITCH_RAT_TO_REAL
            TMP = SQRT((SUMY2-SUMY**2/NN)/NN)
            CALL PUSH_STACK(TMP)
         END IF
   END SELECT

ELSE
   WRITE (UNIT=*, FMT='(A)') '  Input error:  "'//TRIM(STR)//'"'
END IF

END SUBROUTINE EVAL

end module ui
