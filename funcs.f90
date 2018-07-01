module funcs
use assert, only: wp
implicit none

interface csc
  module procedure csc_r, csc_c
end interface csc

interface acsc
  module procedure acsc_r, acsc_c
end interface acsc

interface sec
  module procedure sec_r, sec_c
end interface sec

interface asec
  module procedure asec_r, asec_c
end interface asec

interface cot
  module procedure cot_r, cot_c
end interface cot

interface acot
  module procedure acot_r, acot_c
end interface acot

interface cuberoot
  module procedure cuberoot_r, cuberoot_c
end interface cuberoot

interface hav
  module procedure hav_r, hav_c
end interface hav

interface ahav
  module procedure ahav_r, ahav_c
end interface ahav

interface crd
  module procedure crd_r, crd_c
end interface crd

interface frac
  module procedure frac_r, frac_c
end interface frac

real(wp), parameter :: xinf = huge(0._wp), xmax = xinf, enten=xinf, xmin = tiny(0._wp)

contains
!***********************************************************************************************************************************
!  ISDIGIT
!
!  Determines whether the input character is a digit (0-9).
!***********************************************************************************************************************************

elemental logical FUNCTION ISDIGIT (CH)
CHARACTER, INTENT(IN) :: CH

isdigit = (CH .GE. '0') .AND. (CH .LE. '9')

END FUNCTION ISDIGIT

!***********************************************************************************************************************************
!  ISPM
!
!  Determines whether the input character is a + or - sign.
!***********************************************************************************************************************************

elemental logical FUNCTION ISPM (CH)
CHARACTER, INTENT(IN) :: CH

ispm = (CH .EQ. '+') .OR. (CH .EQ. '-')

END FUNCTION ISPM

!***********************************************************************************************************************************
!  ISHEX
!
!  Determines whether the input character is a valid hexadecimal digit
!***********************************************************************************************************************************

elemental logical FUNCTION ISHEX (CH) 
CHARACTER, INTENT(IN) :: CH

ishex = (((CH .GE. '0') .AND. (CH .LE. '9')) .OR. ((CH .GE. 'A') .AND. (CH .LE. 'F')))

END FUNCTION ISHEX

!***********************************************************************************************************************************
!  ISREAL
!
!  Determines whether the input string is a number.  If it is, it will also return its value.
!
!  In decimal mode, the input string is a number if it satisfies all of these criteria:
!
!     1.  The first non-blank character is a digit, +, -, or decimal point (.).
!     2.  It contains, at most, one decimal point.
!     3.  It contains, at most, one occurrence of E.
!     4.  Any decimal point occurs before any E.
!     5.  The E may be followed by an optional + or -.
!     6.  A letter E must be followed by at least one digit (after the optional + or -).
!     7.  + or - may only be the first non-blank character, or the character immediately following E.
!     8.  It contains no other characters besides: digits, +, -, decimal point (.), and E.
!
!  If not in decimal mode, the input string is a number if it satisfies these criteria:
!
!     1.  BIN:  digits 0 and 1 only
!     2.  OCT:  digits 0-7 only
!     3.  HEX:  digits 0-9 and A-F only
!
!  Uses functions:  ISDIGIT, ISPM, ISHEX
!***********************************************************************************************************************************

      logical FUNCTION ISREAL (STR, X) RESULT (NUM_FLAG)

      USE GLOBAL


      CHARACTER(LEN=*), INTENT(IN) :: STR
      real(wp), INTENT(OUT) :: X
      INTEGER :: I, IERR, ITMP, DPIDX, EIDX, TMPIDX, LENSTR
      LOGICAL :: DPFOUND, EFOUND
      CHARACTER :: CH

      NUM_FLAG = .TRUE.
      LENSTR = LEN_TRIM(STR)

      IF (BASE_MODE .NE. 10) GO TO 100

      CH = STR(1:1)
      IF ((.NOT.ISPM(CH)).AND.(CH.NE.'.').AND.(.NOT.ISDIGIT(CH))) THEN              ! first character not +, -, ., or 0-9
         NUM_FLAG = .FALSE.
         RETURN
      END IF
      IF (ISPM(CH).AND.(LENSTR.EQ.1)) THEN                                          ! + or - is the only character
         NUM_FLAG = .FALSE.
         RETURN
      END IF
      IF ((CH.EQ.'.').AND.(LENSTR.EQ.1)) THEN                                       ! . is the only character
         NUM_FLAG = .FALSE.
         RETURN
      END IF
      IF ((CH.EQ.'E').AND.(LENSTR.EQ.1)) THEN                                       ! E is the only character
         NUM_FLAG = .FALSE.
         RETURN
      END IF

      DPFOUND = CH .EQ. '.'
      EFOUND = .FALSE.

      DO I = 2, LENSTR
         CH = STR(I:I)
         IF (ISDIGIT(CH)) CYCLE                                                     ! digit 0-9 OK anywhere
         IF ((.NOT.ISPM(CH)).AND.(CH.NE.'.').AND.(.NOT.ISDIGIT(CH)).AND. &          ! invalid character
               (CH.NE.'E')) THEN
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (DPFOUND .AND. (CH.EQ.'.')) THEN                                        ! more than one decimal point
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (EFOUND .AND. (CH.EQ.'.')) THEN                                         ! decimal point after E
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (EFOUND .AND. (CH.EQ.'E')) THEN                                         ! more than one E
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (ISPM(CH).AND.(STR(I-1:I-1).NE.'E')) THEN                               ! + or - must be preceded by E
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (CH.EQ.'E') THEN
            IF (I .EQ. LENSTR) THEN                                                 ! E is the last character
               NUM_FLAG = .FALSE.
               RETURN
            END IF
            IF (ISPM(STR(I+1:I+1))) THEN
               IF (I .EQ. LENSTR-1) THEN                                            ! E+ or E- are the last two characters
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
               IF (.NOT.ISDIGIT(STR(I+2:I+2))) THEN                                 ! E+ or E- not followed by a digit
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            ELSE
               IF (.NOT.ISDIGIT(STR(I+1:I+1))) THEN                                 ! E not followed by a digit
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END IF
         END IF
         DPFOUND = DPFOUND .OR. (CH .EQ. '.')
         EFOUND = EFOUND .OR. (CH .EQ. 'E')
      END DO

      READ (UNIT=STR, FMT=*, IOSTAT=IERR) X

      NUM_FLAG = IERR .EQ. 0

      RETURN

  100 SELECT CASE (BASE_MODE)
         CASE (2)                                                                   ! BIN mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF ((CH.NE.'0').AND.(CH.NE.'1')) THEN                                ! only 0 and 1 allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            READ (UNIT=STR, FMT='(B30)', IOSTAT=IERR) ITMP
            X = real(itmp, wp)
            NUM_FLAG = IERR .EQ. 0
         CASE (8)                                                                   ! OCT mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF ((CH.LT.'0').OR.(CH.GT.'7')) THEN                                 ! only 0-7 allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            READ (UNIT=STR, FMT='(O30)', IOSTAT=IERR) ITMP
            X = real(itmp, wp)
            NUM_FLAG = IERR .EQ. 0
         CASE (16)                                                                  ! HEX mode
            IF (STR.EQ.'DEC') THEN                                                  !   DEC is a valid hex integer, so check..
               NUM_FLAG = .FALSE.                                                   !   ..if we're switching to DEC mode
               RETURN                                                               !   (enter 0DEC to get the hex integer DEC)
            END IF
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF (.NOT.ISHEX(CH)) THEN                                             !   only 0-9 and A-F allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            READ (UNIT=STR, FMT='(Z30)', IOSTAT=IERR) ITMP
            X = real(itmp, wp)
            NUM_FLAG = IERR .EQ. 0
      END SELECT

      RETURN

      END FUNCTION ISREAL





!***********************************************************************************************************************************
!  ISCOMPLEX
!
!  Determines whether the input string is a complex number.  If it is, it will also return its value.
!  A complex number consists of two real numbers separated by a comma (no space).  For example:  2.0,5.0E2
!
!  In decimal mode, the input string is a number if it satisfies all of these criteria:
!
!     1.  The first non-blank character is a digit, +, -, or decimal point (.).
!     2.  It contains, at most, one decimal point.
!     3.  It contains, at most, one occurrence of E.
!     4.  Any decimal point occurs before any E.
!     5.  The E may be followed by an optional + or -.
!     6.  A letter E must be followed by at least one digit (after the optional + or -).
!     7.  + or - may only be the first non-blank character, or the character immediately following E.
!     8.  It contains no other characters besides: digits, +, -, decimal point (.), and E.
!
!  If not in decimal mode, the input string is a number if it satisfies these criteria:
!
!     1.  BIN:  digits 0 and 1 only
!     2.  OCT:  digits 0-7 only
!     3.  HEX:  digits 0-9 and A-F only
!
!  Uses functions:  ISDIGIT, ISPM, ISHEX
!***********************************************************************************************************************************

      FUNCTION ISCOMPLEX (STR, X) RESULT (NUM_FLAG)

      USE GLOBAL

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: STR
      COMPLEX(wp), INTENT(OUT) :: X
      LOGICAL :: NUM_FLAG
      INTEGER :: I, IERRR, IERRI, DPIDX, EIDX, TMPIDX, LENSTR, COMMAIDX
      INTEGER :: IXR, IXI
      real(wp) :: XR, XI
      LOGICAL :: DPFOUND, EFOUND, COMMAFOUND
      CHARACTER :: CH



      NUM_FLAG = .TRUE.
      LENSTR = LEN_TRIM(STR)

      IF (BASE_MODE .NE. 10) GO TO 100

      CH = STR(1:1)
      IF ((.NOT.ISPM(CH)).AND.(CH.NE.'.').AND.(.NOT.ISDIGIT(CH))) THEN              ! first character not +, -, ., or 0-9
         NUM_FLAG = .FALSE.
         RETURN
      END IF
      IF (ISPM(CH).AND.(LENSTR.EQ.1)) THEN                                          ! + or - is the only character
         NUM_FLAG = .FALSE.
         RETURN
      END IF
      IF ((CH.EQ.'.').AND.(LENSTR.EQ.1)) THEN                                       ! . is the only character
         NUM_FLAG = .FALSE.
         RETURN
      END IF
      IF ((CH.EQ.'E').AND.(LENSTR.EQ.1)) THEN                                       ! E is the only character
         NUM_FLAG = .FALSE.
         RETURN
      END IF

      DPFOUND = CH .EQ. '.'
      EFOUND = .FALSE.
      COMMAFOUND = .FALSE.

      DO I = 2, LENSTR
         CH = STR(I:I)
         IF (ISDIGIT(CH)) CYCLE                                                     ! digit 0-9 OK anywhere
         IF ((.NOT.ISPM(CH)).AND.(CH.NE.'.').AND.(.NOT.ISDIGIT(CH)).AND. &          ! invalid character
               (CH.NE.'E').AND.(CH.NE.',')) THEN
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (DPFOUND .AND. (CH.EQ.'.')) THEN                                        ! more than one decimal point
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (EFOUND .AND. (CH.EQ.'.')) THEN                                         ! decimal point after E
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (EFOUND .AND. (CH.EQ.'E')) THEN                                         ! more than one E
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (ISPM(CH).AND.(STR(I-1:I-1).NE.'E').AND.(STR(I-1:I-1).NE.',')) THEN     ! + or - must be preceded by E or comma
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         IF (CH.EQ.'E') THEN
            IF (I .EQ. LENSTR) THEN                                                 ! E is the last character
               NUM_FLAG = .FALSE.
               RETURN
            END IF
            IF (ISPM(STR(I+1:I+1))) THEN
               IF (I .EQ. LENSTR-1) THEN                                            ! E+ or E- are the last two characters
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
               IF (.NOT.ISDIGIT(STR(I+2:I+2))) THEN                                 ! E+ or E- not followed by a digit
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            ELSE
               IF (.NOT.ISDIGIT(STR(I+1:I+1))) THEN                                 ! E not followed by a digit
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END IF
         END IF
         DPFOUND = DPFOUND .OR. (CH .EQ. '.')
         EFOUND = EFOUND .OR. (CH .EQ. 'E')
         COMMAFOUND = COMMAFOUND .OR. (CH .EQ. ',')
         IF (CH .EQ. ',') THEN
            READ (UNIT=STR(1:I-1), FMT=*, IOSTAT=IERRR) XR
            COMMAIDX = I
            DPFOUND = .FALSE.
            EFOUND = .FALSE.
         END IF
      END DO

      IF (.NOT.COMMAFOUND) THEN
         READ (UNIT=STR(1:I-1), FMT=*, IOSTAT=IERRR) XR
         XI = 0.0D0
         IERRI = 0
      ELSE
         READ (UNIT=STR(COMMAIDX+1:), FMT=*, IOSTAT=IERRI) XI
      END IF

      X = CMPLX(XR,XI, wp)
      NUM_FLAG = (IERRR .EQ. 0) .AND. (IERRI .EQ. 0)

      RETURN

  100 COMMAFOUND = .FALSE.

      IF (STR(1:1) .EQ. ',') THEN                                                   ! first character is a comma
         NUM_FLAG = .FALSE.
         RETURN
      END IF

      SELECT CASE (BASE_MODE)
         CASE (2)                                                                   ! BIN mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF (CH .EQ. ',') THEN
                  IF (COMMAFOUND) THEN                                              ! more than one comma found
                     NUM_FLAG = .FALSE.
                     RETURN
                  END IF
                  READ (UNIT=STR(1:I-1), FMT='(B50)', IOSTAT=IERRR) IXR
                  COMMAFOUND = .TRUE.
                  COMMAIDX = I
               ELSE IF ((CH.NE.'0').AND.(CH.NE.'1')) THEN                           ! only 0 and 1 allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            IF (.NOT. COMMAFOUND) THEN
               READ (UNIT=STR(1:I-1), FMT='(B50)', IOSTAT=IERRR) IXR
               IXI = 0
               IERRI = 0
            ELSE
               READ (UNIT=STR(COMMAIDX+1:), FMT='(B50)', IOSTAT=IERRI) IXI
            END IF
            X = CMPLX(real(ixr, wp),real(ixi, wp), wp)
            NUM_FLAG = (IERRR .EQ. 0) .AND. (IERRI .EQ. 0)
         CASE (8)                                                                   ! OCT mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF (CH .EQ. ',') THEN
                  IF (COMMAFOUND) THEN                                              ! more than one comma found
                     NUM_FLAG = .FALSE.
                     RETURN
                  END IF
                  READ (UNIT=STR(1:I-1), FMT='(O50)', IOSTAT=IERRR) IXR
                  COMMAFOUND = .TRUE.
                  COMMAIDX = I
               ELSE IF ((CH.LT.'0').OR.(CH.GT.'7')) THEN                            ! only 0-7 allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            IF (.NOT. COMMAFOUND) THEN
               READ (UNIT=STR(1:I-1), FMT='(O50)', IOSTAT=IERRR) IXR
               IXI = 0
               IERRI = 0
            ELSE
               READ (UNIT=STR(COMMAIDX+1:), FMT='(O50)', IOSTAT=IERRI) IXI
            END IF
            X = CMPLX(real(ixr, wp),real(ixi, wp), wp)
            NUM_FLAG = (IERRR .EQ. 0) .AND. (IERRI .EQ. 0)
         CASE (16)                                                                  ! HEX mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF (CH .EQ. ',') THEN
                  IF (COMMAFOUND) THEN                                              ! more than one comma found
                     NUM_FLAG = .FALSE.
                     RETURN
                  END IF
                  IF (STR(1:I-1) .EQ. 'DEC') THEN                                   !   DEC is a valid hex integer, so check..
                     NUM_FLAG = .FALSE.                                             !   ..if we're switching to DEC mode
                     RETURN                                                         !   (enter 0DEC to get the hex integer DEC)
                  END IF
                  READ (UNIT=STR(1:I-1), FMT='(Z50)', IOSTAT=IERRR) IXR
                  COMMAFOUND = .TRUE.
                  COMMAIDX = I
               ELSE IF (.NOT.ISHEX(CH)) THEN                                        ! only hex digits allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            IF (.NOT. COMMAFOUND) THEN
               IF (STR(1:I-1) .EQ. 'DEC') THEN                                      !   DEC is a valid hex integer, so check..
                  NUM_FLAG = .FALSE.                                                !   ..if we're switching to DEC mode
                  RETURN                                                            !   (enter 0DEC to get the hex integer DEC)
               END IF
               READ (UNIT=STR(1:I-1), FMT='(Z50)', IOSTAT=IERRR) IXR
               IXI = 0
               IERRI = 0
            ELSE
               IF (STR(1:I-1) .EQ. 'DEC') THEN                                      !   DEC is a valid hex integer, so check..
                  NUM_FLAG = .FALSE.                                                !   ..if we're switching to DEC mode
                  RETURN                                                            !   (enter 0DEC to get the hex integer DEC)
               END IF
               READ (UNIT=STR(COMMAIDX+1:), FMT='(Z50)', IOSTAT=IERRI) IXI
            END IF
            X = CMPLX(real(IXR, wp), real(IXI, wp), wp)
            NUM_FLAG = (IERRR .EQ. 0) .AND. (IERRI .EQ. 0)
      END SELECT

      RETURN

      END FUNCTION ISCOMPLEX





!***********************************************************************************************************************************
!  ISRATIONAL
!
!  Determines whether the input string is a rational number.  If it is, it will also return its value.
!  A rational number consists of two integers separated by a slash (no space).  For example:  2/5
!
!  In decimal mode, the input string is a number if it satisfies all of these criteria:
!
!     1.  The first non-blank character is a digit, +, or -.
!     2.  It contains no other characters besides: digits, +, and -.
!
!  If not in decimal mode, the input string is a number if it satisfies these criteria:
!
!     1.  BIN:  digits 0 and 1 only
!     2.  OCT:  digits 0-7 only
!     3.  HEX:  digits 0-9 and A-F only
!
!  Uses functions:  ISDIGIT, ISPM, ISHEX
!***********************************************************************************************************************************

      FUNCTION ISRATIONAL (STR, NUM, DEN) RESULT (NUM_FLAG)

      USE GLOBAL


      CHARACTER(LEN=*), INTENT(IN) :: STR
      INTEGER, INTENT(OUT) :: NUM, DEN
      LOGICAL :: NUM_FLAG
      INTEGER :: I, IERRN, IERRD, LENSTR, SLASHIDX
      LOGICAL :: SLASHFOUND
      CHARACTER :: CH


      NUM_FLAG = .TRUE.
      LENSTR = LEN_TRIM(STR)

      IF (BASE_MODE .NE. 10) GO TO 100

      CH = STR(1:1)
      IF ((.NOT.ISPM(CH)).AND.(.NOT.ISDIGIT(CH))) THEN                              ! first character not +, -, or 0-9
         NUM_FLAG = .FALSE.
         RETURN
      END IF
      IF (ISPM(CH).AND.(LENSTR.EQ.1)) THEN                                          ! + or - is the only character
         NUM_FLAG = .FALSE.
         RETURN
      END IF

      SLASHFOUND = .FALSE.

      DO I = 2, LENSTR
         CH = STR(I:I)
         IF (ISDIGIT(CH)) CYCLE                                                     ! digit 0-9 OK anywhere
         IF ((.NOT.ISDIGIT(CH)).AND.(CH.NE.'/')) THEN                               ! invalid character
            NUM_FLAG = .FALSE.
            RETURN
         END IF
         SLASHFOUND = SLASHFOUND .OR. (CH .EQ. '/')
         IF (CH .EQ. '/') THEN
            READ (UNIT=STR(1:I-1), FMT=*, IOSTAT=IERRN) NUM
            SLASHIDX = I
         END IF
      END DO

      IF (.NOT.SLASHFOUND) THEN
         READ (UNIT=STR(1:I-1), FMT=*, IOSTAT=IERRN) NUM
         DEN = 1
         IERRD = 0
      ELSE
         READ (UNIT=STR(SLASHIDX+1:), FMT=*, IOSTAT=IERRD) DEN
      END IF

      NUM_FLAG = (IERRN .EQ. 0) .AND. (IERRD .EQ. 0)

      IF (NUM_FLAG) CALL RATNORM (NUM, DEN)

      RETURN

  100 SLASHFOUND = .FALSE.

      IF (STR(1:1) .EQ. '/') THEN                                                   ! first character is a slash
         NUM_FLAG = .FALSE.
         RETURN
      END IF

      SELECT CASE (BASE_MODE)
         CASE (2)                                                                   ! BIN mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF (CH .EQ. '/') THEN
                  IF (SLASHFOUND) THEN                                              ! more than one slash found
                     NUM_FLAG = .FALSE.
                     RETURN
                  END IF
                  READ (UNIT=STR(1:I-1), FMT='(B50)', IOSTAT=IERRN) NUM
                  SLASHFOUND = .TRUE.
                  SLASHIDX = I
               ELSE IF ((CH.NE.'0').AND.(CH.NE.'1')) THEN                           ! only 0 and 1 allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            IF (.NOT. SLASHFOUND) THEN
               READ (UNIT=STR(1:I-1), FMT='(B50)', IOSTAT=IERRN) NUM
               DEN = 1
               IERRD = 0
            ELSE
               READ (UNIT=STR(SLASHIDX+1:), FMT='(B50)', IOSTAT=IERRD) DEN
            END IF
            NUM_FLAG = (IERRN .EQ. 0) .AND. (IERRD .EQ. 0)
            IF (NUM_FLAG) CALL RATNORM (NUM, DEN)
         CASE (8)                                                                   ! OCT mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF (CH .EQ. '/') THEN
                  IF (SLASHFOUND) THEN                                              ! more than one slash found
                     NUM_FLAG = .FALSE.
                     RETURN
                  END IF
                  READ (UNIT=STR(1:I-1), FMT='(O50)', IOSTAT=IERRN) NUM
                  SLASHFOUND = .TRUE.
                  SLASHIDX = I
               ELSE IF ((CH.LT.'0').OR.(CH.GT.'7')) THEN                            ! only 0-7 allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            IF (.NOT. SLASHFOUND) THEN
               READ (UNIT=STR(1:I-1), FMT='(O50)', IOSTAT=IERRN) NUM
               DEN = 1
               IERRD = 0
            ELSE
               READ (UNIT=STR(SLASHIDX+1:), FMT='(O50)', IOSTAT=IERRD) DEN
            END IF
            NUM_FLAG = (IERRN .EQ. 0) .AND. (IERRD .EQ. 0)
            IF (NUM_FLAG) CALL RATNORM (NUM, DEN)
         CASE (16)                                                                  ! HEX mode
            DO I = 1, LENSTR
               CH = STR(I:I)
               IF (CH .EQ. '/') THEN
                  IF (SLASHFOUND) THEN                                              ! more than one slash found
                     NUM_FLAG = .FALSE.
                     RETURN
                  END IF
                  IF (STR(1:I-1) .EQ. 'DEC') THEN                                   !   DEC is a valid hex integer, so check..
                     NUM_FLAG = .FALSE.                                             !   ..if we're switching to DEC mode
                     RETURN                                                         !   (enter 0DEC to get the hex integer DEC)
                  END IF
                  READ (UNIT=STR(1:I-1), FMT='(Z50)', IOSTAT=IERRN) NUM
                  SLASHFOUND = .TRUE.
                  SLASHIDX = I
               ELSE IF (.NOT.ISHEX(CH)) THEN                                        ! only hex digits allowed
                  NUM_FLAG = .FALSE.
                  RETURN
               END IF
            END DO
            IF (.NOT. SLASHFOUND) THEN
               IF (STR(1:I-1) .EQ. 'DEC') THEN                                      !   DEC is a valid hex integer, so check..
                  NUM_FLAG = .FALSE.                                                !   ..if we're switching to DEC mode
                  RETURN                                                            !   (enter 0DEC to get the hex integer DEC)
               END IF
               READ (UNIT=STR(1:I-1), FMT='(Z50)', IOSTAT=IERRN) NUM
               DEN = 1
               IERRD = 0
            ELSE
               IF (STR(1:I-1) .EQ. 'DEC') THEN                                      !   DEC is a valid hex integer, so check..
                  NUM_FLAG = .FALSE.                                                !   ..if we're switching to DEC mode
                  RETURN                                                            !   (enter 0DEC to get the hex integer DEC)
               END IF
               READ (UNIT=STR(SLASHIDX+1:), FMT='(Z50)', IOSTAT=IERRD) DEN
            END IF
            NUM_FLAG = (IERRN .EQ. 0) .AND. (IERRD .EQ. 0)
            IF (NUM_FLAG) CALL RATNORM (NUM, DEN)
      END SELECT

      END FUNCTION ISRATIONAL





!***********************************************************************************************************************************
!  SWITCH_RAT_TO_REAL
!***********************************************************************************************************************************

SUBROUTINE SWITCH_RAT_TO_REAL

USE GLOBAL

INTEGER :: I

DOMAIN_MODE = 1

DO I = 1, STACK_SIZE
   STACK(I) = DBLE(RNSTACK(I))/DBLE(RDSTACK(I))
END DO

DO I = 0, REG_SIZE-1
   REG(I) = DBLE(RNREG(I))/DBLE(RDREG(I))
END DO

LASTX = DBLE(RNLASTX)/DBLE(RDLASTX)

NN = DBLE(RNNN)/DBLE(RDNN)
SUMX = DBLE(RNSUMX)/DBLE(RDSUMX)
SUMX2 = DBLE(RNSUMX2)/DBLE(RDSUMX2)
SUMY = DBLE(RNSUMY)/DBLE(RDSUMY)
SUMY2 = DBLE(RNSUMY2)/DBLE(RDSUMY2)
SUMXY = DBLE(RNSUMXY)/DBLE(RDSUMXY)

END SUBROUTINE SWITCH_RAT_TO_REAL





!***********************************************************************************************************************************
!  MATHEMATICAL FUNCTIONS
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!  GCD
!
!  Greatest common divisor.
!  Find the greatest common divisor of two integers using Euclid's algorithm.
!***********************************************************************************************************************************

elemental integer FUNCTION GCD (A, B)

INTEGER, INTENT(IN) :: A, B

INTEGER :: A1, B1, T

A1 = A
B1 = B

DO WHILE (B1 .NE. 0)
   T = B1
   B1 = MOD (A1,B1)
   A1 = T
END DO

GCD = A1

END FUNCTION GCD


!***********************************************************************************************************************************
!  LCM
!
!  Least common multiple.
!  Find the least common multiple of two integers.
!***********************************************************************************************************************************

elemental integer FUNCTION LCM (A, B)

INTEGER, INTENT(IN) :: A, B

LCM = A*B/GCD(A,B)

END FUNCTION LCM



!***********************************************************************************************************************************
!  RATNORM
!
!  Normalize a rational number
!  Check for a denominator or numerator of 0; make the denominator positive; and reduce the fraction.
!***********************************************************************************************************************************

elemental SUBROUTINE RATNORM (NUM, DEN)

INTEGER, INTENT(IN OUT) :: NUM, DEN

INTEGER :: G
LOGICAL :: NEGFLAG

IF (DEN .EQ. 0) THEN                                                        ! check for zero denominator
   error stop  'Error in RATNORM: denominator is zero.'
   NUM = 0
   DEN = 1
   RETURN
END IF

IF (NUM .EQ. 0) THEN                                                        ! if zero numerator, just return 0/1
   NUM = 0
   DEN = 1
   RETURN
END IF

NEGFLAG = (NUM .LT. 0) .NEQV. (DEN .LT. 0)                                ! save sign of fraction in NEGFLAG

NUM = ABS(NUM)                                                            ! take absolute value of NUM and DEN
DEN = ABS(DEN)

G = GCD (NUM, DEN)                                                        ! find GCD of NUM and DEN

NUM = NUM / G                                                             ! reduce the fraction
DEN = DEN / G

IF (NEGFLAG) NUM = -NUM                                                   ! restore the sign to the numerator

END SUBROUTINE RATNORM





!***********************************************************************************************************************************
!  RADD
!
!  Add two rational numbers.
!***********************************************************************************************************************************

elemental SUBROUTINE RADD (N1, D1, N2, D2, NR, DR)

INTEGER, INTENT(IN) :: N1, D1, N2, D2
INTEGER, INTENT(OUT) :: NR, DR

NR = N1*D2+D1*N2
DR = D1*D2
CALL RATNORM (NR, DR)

END SUBROUTINE RADD





!***********************************************************************************************************************************
!  RSUB
!
!  Subtract two rational numbers.
!***********************************************************************************************************************************

elemental SUBROUTINE RSUB (N1, D1, N2, D2, NR, DR)

INTEGER, INTENT(IN) :: N1, D1, N2, D2
INTEGER, INTENT(OUT) :: NR, DR

NR = N1*D2-D1*N2
DR = D1*D2
CALL RATNORM (NR, DR)

END SUBROUTINE RSUB





!***********************************************************************************************************************************
!  RMUL
!
!  Multiply two rational numbers.
!***********************************************************************************************************************************

elemental SUBROUTINE RMUL (N1, D1, N2, D2, NR, DR)

INTEGER, INTENT(IN) :: N1, D1, N2, D2
INTEGER, INTENT(OUT) :: NR, DR

NR = N1*N2
DR = D1*D2
CALL RATNORM (NR, DR)

END SUBROUTINE RMUL





!***********************************************************************************************************************************
!  RDIV
!
!  Multiply two rational numbers.
!***********************************************************************************************************************************

elemental SUBROUTINE RDIV (N1, D1, N2, D2, NR, DR)

INTEGER, INTENT(IN) :: N1, D1, N2, D2
INTEGER, INTENT(OUT) :: NR, DR

NR = N1*D2
DR = D1*N2
CALL RATNORM (NR, DR)

END SUBROUTINE RDIV





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


elemental complex(wp) FUNCTION FRAC_c (X) result(frac)

COMPLEX(wp), INTENT(IN) :: X
real(wp) :: XR, XI, YR, YI, ZR, ZI

XR = real(X, wp)
XI = AIMAG(X)

ZR = ABS(XR)
YR = ZR - INT(ZR)
YR = SIGN(YR,XR)

ZI = ABS(XI)
YI = ZI - INT(ZI)
YI = SIGN(YI,XI)

FRAC = CMPLX(YR,YI, wp)

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

YR = AINT(DBLE(X))
YI = AINT(AIMAG(X))
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


!***********************************************************************************************************************************
!  CNR
!
!  Combinations of N things taken R at a time.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CNR (N,R) RESULT (Y)

INTEGER, INTENT(IN) :: N, R
INTEGER :: I, J

Y = 1._wp
J = N

DO I = N-R, 1, -1
   Y = Y * real(j,wp)/real(i,wp)
   J = J - 1
END DO

END FUNCTION CNR

!***********************************************************************************************************************************
!  PNR
!
!  Permutations of N things taken R at a time.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION PNR (N,R) RESULT (Y)

INTEGER, INTENT(IN) :: N, R
INTEGER :: I, J

Y = 1._wp
J = N

DO I = N-R, 1, -1
   Y = Y * real(j,wp)/real(i,wp)
   J = J - 1
END DO

DO I = R, 1, -1
   Y = Y * real(i,wp)
END DO


END FUNCTION PNR



!***********************************************************************************************************************************
!  SEC
!
!  Secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION SEC_r (X) RESULT (sec)
real(wp), INTENT (IN) :: X

sec = 1._wp/COS(X)
END FUNCTION SEC_r


elemental complex(wp) FUNCTION SEC_c(Z) result(sec)
COMPLEX(wp), INTENT(IN) :: Z

SEC = 1._wp/COS(Z)
END FUNCTION SEC_c



!***********************************************************************************************************************************
!  ASEC
!
!  Inverse secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ASEC_r (Y) RESULT (X)
real(wp), INTENT (IN) :: Y

X = ACOS(1._wp/Y)
END FUNCTION ASEC_r


elemental complex(wp) FUNCTION ASEC_c (Z) RESULT (Y)
COMPLEX(wp), INTENT(IN) :: Z

Y = ACOS(1._wp/Z)
END FUNCTION ASEC_c
!***********************************************************************************************************************************
!  CSC
!
!  Cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CSC_r(X) result(csc)
real(wp), INTENT (IN) :: X

 CSC = 1._wp/SIN(X)
END FUNCTION CSC_r


elemental complex(wp) FUNCTION CSC_c(Z) result(csc)
COMPLEX(wp), INTENT(IN) :: Z

 CSC = 1._wp/SIN(Z)

END FUNCTION CSC_c


!***********************************************************************************************************************************
!  ACSC
!
!  Inverse cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACSC_r(Y) result(acsc)
real(wp), INTENT (IN) :: Y

ACSC = ASIN(1._wp/Y)
END FUNCTION ACSC_r


elemental complex(wp) FUNCTION ACSC_c(Z) RESULT(acsc)
COMPLEX(wp), INTENT(IN) :: Z

acsc = ASIN(1._wp/Z)

END FUNCTION ACSC_c


!***********************************************************************************************************************************
!  COT
!
!  Cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION COT_r(X) result(cot)
real(wp), INTENT (IN) :: X

 COT = 1._wp/TAN(X)
END FUNCTION COT_r


elemental complex(wp) FUNCTION COT_c(Z) result(cot)
COMPLEX(wp), INTENT(IN) :: Z

 COT = COS(Z)/SIN(Z)

END FUNCTION COT_c

!***********************************************************************************************************************************
!  ACOT
!
!  Inverse cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACOT_r (Y) result(acot)
real(wp), INTENT (IN) :: Y

ACOT = ATAN(1._wp/Y)

END FUNCTION ACOT_r


elemental complex(wp) FUNCTION ACOT_c(Y) result (Acot)
complex(wp), INTENT (IN) :: Y

ACOT = ATAN(1._wp/Y)
END FUNCTION ACOT_c

!***********************************************************************************************************************************
!  ACOT2
!
!  Inverse cotangent (two arguments).
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACOT2 (Y,Z) 

real(wp), INTENT (IN) :: Y                                           ! cotangent numerator
real(wp), INTENT (IN) :: Z                                           ! cotangent denominator

ACOT2 = ATAN2(Z,Y)

END FUNCTION ACOT2


!***********************************************************************************************************************************
!  EXSEC
!
!  Exsecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION EXSEC (X)
real(wp), INTENT (IN) :: X
 
EXSEC = 1._wp/COS(X) - 1._wp

END FUNCTION EXSEC

!***********************************************************************************************************************************
!  CEXSEC
!
!  Complex exsecant.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CEXSEC (Z)

COMPLEX(wp), INTENT(IN) :: Z
 cexsec = 1._wp/COS(Z) - 1._wp

END FUNCTION CEXSEC

!***********************************************************************************************************************************
!  AEXSEC
!
!  Inverse exsecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION AEXSEC (Y)
real(wp), INTENT (IN) :: Y

AEXSEC = ACOS(1._wp / (Y + 1._wp))

END FUNCTION AEXSEC

!***********************************************************************************************************************************
!  CAEXSEC
!
!  Complex inverse exsecant.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CAEXSEC (Y) RESULT (X)

COMPLEX(wp), INTENT (IN) :: Y

X = ACOS(1._wp / (Y + 1._wp))

END FUNCTION CAEXSEC


!***********************************************************************************************************************************
!  VERS
!
!  Versine.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION VERS (X) 
real(wp), INTENT (IN) :: X

VERS = 1._wp - COS(X)

END FUNCTION VERS

!***********************************************************************************************************************************
!  CVERS
!
!  Complex versine.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CVERS (Z)
COMPLEX(wp), INTENT(IN) :: Z

 CVERS = 1._wp - COS(Z)

END FUNCTION CVERS

!***********************************************************************************************************************************
!  AVERS
!
!  Inverse versine.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION AVERS (Y)
real(wp), INTENT (IN) :: Y

AVERS = ACOS(1._wp - Y)

END FUNCTION AVERS

!***********************************************************************************************************************************
!  CAVERS
!
!  Complex inverse versine.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CAVERS (Y) RESULT (X)

COMPLEX(wp), INTENT (IN) :: Y

X = acos(1._wp - Y)

END FUNCTION CAVERS


!*************************************************************************************************
!  COVERS
!
!  Coversine.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION COVERS (X) RESULT (Y)

real(wp), INTENT (IN) :: X

Y = 1._wp - SIN(X)

END FUNCTION COVERS


!***********************************************************************************************************************************
!  CCOVERS
!
!  Complex coversine.
!***********************************************************************************************************************************

FUNCTION CCOVERS (Z) RESULT (Y)

COMPLEX(wp), INTENT(IN) :: Z
COMPLEX(wp) :: Y

Y = 1._wp - SIN(Z)

END FUNCTION CCOVERS





!***********************************************************************************************************************************
!  ACOVERS
!
!  Inverse coversine.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACOVERS (Y) RESULT (X)

real(wp), INTENT (IN) :: Y

X = ASIN(1._wp - Y)

END FUNCTION ACOVERS





!***********************************************************************************************************************************
!  CACOVERS
!
!  Complex inverse coversine.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CACOVERS (Y) RESULT (X)

COMPLEX(wp), INTENT (IN) :: Y

X = ASIN(1._wp - Y)

END FUNCTION CACOVERS


!***********************************************************************************************************************************
!  HAV
!
!  Haversine.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION HAV_r(X) RESULT (Y)
real(wp), INTENT (IN) :: X

Y = (SIN(0.5D0*X))**2
END FUNCTION HAV_r


elemental complex(wp) FUNCTION HAV_c(Z) RESULT (Y)
COMPLEX(wp), INTENT(IN) :: Z

Y = (SIN(0.5D0*Z))**2
END FUNCTION HAV_c



!***********************************************************************************************************************************
!  AHAV
!
!  Inverse haversine.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION AHAV_r(Y) RESULT (X)

real(wp), INTENT (IN) :: Y

X = 2._wp*ASIN(SQRT(Y))
END FUNCTION AHAV_r


elemental complex(wp) FUNCTION AHAV_c(Y) RESULT (X)
COMPLEX(wp), INTENT (IN) :: Y

X = 2._wp*asin(SQRT(Y))
END FUNCTION AHAV_c


!***********************************************************************************************************************************
!  CRD
!
!  Chord (of Ptolemy).
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CRD_r (X) result(crd)
real(wp), INTENT (IN) :: X

 CRD = 2._wp*SIN(0.5_wp*X)

END FUNCTION CRD_r


elemental complex(wp) FUNCTION CRD_c (Z) RESULT (crd)
COMPLEX(wp), INTENT(IN) :: Z

 CRD = 2._wp*SIN(0.5_wp*Z)

END FUNCTION CRD_c



!***********************************************************************************************************************************
!  ACRD
!
!  Inverse chord (of Ptolemy).
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACRD (Y) RESULT (X)

real(wp), INTENT (IN) :: Y

X = 2.0D0*ASIN(0.5D0*Y)

END FUNCTION ACRD
!***********************************************************************************************************************************
!  CACRD
!
!  Complex inverse chord (of Ptolemy).
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CACRD (Y) RESULT (X)

COMPLEX(wp), INTENT (IN) :: Y

X = 2.0D0*asin(0.5D0*Y)

END FUNCTION CACRD

!***********************************************************************************************************************************
!  LOG1P
!
!  Compute log(1+x).
!***********************************************************************************************************************************

elemental real(wp) FUNCTION LOG1P (X) RESULT (Y)
real(wp), INTENT(IN) :: X
real(wp) :: Z

Z = 1._wp + X
Y = LOG(Z) - ((Z-1.0D0)-X)/Z                                                  ! cancels errors with IEEE arithmetic

END FUNCTION LOG1P



!***********************************************************************************************************************************
!  SECH
!
!  Hyperbolic secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION SECH (X)

real(wp), INTENT (IN) :: X

sech = 1._wp/COSH(X)

END FUNCTION SECH



!***********************************************************************************************************************************
!  CSECH
!
!  Complex hyperbolic secant.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CSECH (Z) RESULT (Y)
COMPLEX(wp), INTENT (IN) :: Z

Y = 1._wp/cosh(Z)

END FUNCTION CSECH



!***********************************************************************************************************************************
!  ASECH
!
!  Inverse hyperbolic secant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ASECH (Y)
real(wp), INTENT (IN) :: Y

asech = ACOSH(1._wp/Y)

END FUNCTION ASECH





!***********************************************************************************************************************************
!  CASECH
!
!  Complex inverse hyperbolic secant.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CASECH(Y)
COMPLEX(wp), INTENT (IN) :: Y

casech = ACOSH(1._wp/Y)

END FUNCTION CASECH

!***********************************************************************************************************************************
!  CSCH
!
!  Hyperbolic cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CSCH (X) RESULT (Y)

real(wp), INTENT (IN) :: X

Y = 1._wp/SINH(X)

END FUNCTION CSCH

!***********************************************************************************************************************************
!  CCSCH
!
!  Complex hyperbolic cosecant.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CCSCH (Z) RESULT (Y)

COMPLEX(wp), INTENT (IN) :: Z

Y = 1._wp / SINH(Z)

END FUNCTION CCSCH

!***********************************************************************************************************************************
!  ACSCH
!
!  Inverse hyperbolic cosecant.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACSCH (Y) RESULT (X)

real(wp), INTENT (IN) :: Y

X = ASINH(1._wp/Y)


END FUNCTION ACSCH

!***********************************************************************************************************************************
!  CACSCH
!
!  Complex inverse hyperbolic cosecant.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CACSCH (Y) RESULT (X)

COMPLEX(wp), INTENT (IN) :: Y

X = ASINH(1._wp/Y)

END FUNCTION CACSCH





!***********************************************************************************************************************************
!  COTH
!
!  Hyperbolic cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION COTH (X)

real(wp), INTENT (IN) :: X

coth = 1._wp/TANH(X)


END FUNCTION COTH





!***********************************************************************************************************************************
!  CCOTH
!
!  Complex hyperbolic cotangent.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CCOTH (Z)

IMPLICIT NONE

COMPLEX(wp), INTENT (IN) :: Z
COMPLEX(wp) :: Y

 CCOTH = 1._wp / tanh(Z)


END FUNCTION CCOTH





!***********************************************************************************************************************************
!  ACOTH
!
!  Inverse hyperbolic cotangent.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION ACOTH (Y)
real(wp), INTENT (IN) :: Y

ACOTH = ATANH(1._wp/Y)

END FUNCTION ACOTH



!***********************************************************************************************************************************
!  CACOTH
!
!  Complex inverse hyperbolic cotangent.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CACOTH (Z)
COMPLEX(wp), INTENT(IN) :: Z

cacoth = 0.5D0*LOG((Z+1.0D0)/(Z-1.0D0))

END FUNCTION CACOTH


!***********************************************************************************************************************************
!  SINC
!
!  Sine cardinal (sinc) function.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION SINC (X) RESULT (Y)

real(wp), INTENT(IN) :: X

IF (X .EQ. 0.0D0) THEN
   Y = 1._wp
ELSE
   Y = SIN(X)/X
END IF


END FUNCTION SINC





!***********************************************************************************************************************************
!  CSINC
!
!  Complex sine cardinal (sinc) function.
!***********************************************************************************************************************************

FUNCTION CSINC (Z) RESULT (Y)

IMPLICIT NONE

COMPLEX(wp), INTENT(IN) :: Z
COMPLEX(wp) :: Y

IF (Z .EQ. (0.0D0,0.0D0)) THEN
   Y = (1._wp,0.0D0)
ELSE
   Y = SIN(Z)/Z
END IF

RETURN

END FUNCTION CSINC





!***********************************************************************************************************************************
!  TANC
!
!  Tanc function.
!***********************************************************************************************************************************

FUNCTION TANC (X) RESULT (Y)

IMPLICIT NONE

real(wp), INTENT(IN) :: X
real(wp) :: Y

IF (X .EQ. 0.0D0) THEN
   Y = 1._wp
ELSE
   Y = TAN(X)/X
END IF

RETURN

END FUNCTION TANC



!***********************************************************************************************************************************
!  CTANC
!
!  Complex tanc function.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CTANC (Z) RESULT (Y)

COMPLEX(wp), INTENT(IN) :: Z

IF (Z .EQ. (0.0D0,0.0D0)) THEN
   Y = (1._wp,0.0D0)
ELSE
   Y = TAN(Z)/Z
END IF

END FUNCTION CTANC



!***********************************************************************************************************************************
!  SINHC
!
!  Sinhc function.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION SINHC (X) RESULT (Y)

real(wp), INTENT(IN) :: X

IF (X .EQ. 0.0D0) THEN
   Y = 1._wp
ELSE
   Y = SINH(X)/X
END IF

END FUNCTION SINHC



!***********************************************************************************************************************************
!  CSINHC
!
!  Complex sinhc function.
!***********************************************************************************************************************************

elemental complex(wp) FUNCTION CSINHC (Z) RESULT (Y)
COMPLEX(wp), INTENT(IN) :: Z


IF (Z .EQ. (0.0D0,0.0D0)) THEN
   Y = (1._wp,0.0D0)
ELSE
   Y = SINH(Z)/Z
END IF

END FUNCTION CSINHC





!***********************************************************************************************************************************
!  TANHC
!
!  Tanhc function.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION TANHC (X) RESULT (Y)

real(wp), INTENT(IN) :: X


IF (X .EQ. 0._wp) THEN
   Y = 1._wp
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
!  PSI
!
!  Returns the digamma function of the argument XX.  XX cannot be 0 or a negative integer.
!
!  From http://www.netlib.org/specfun/gamma
!***********************************************************************************************************************************

      FUNCTION PSI(XX)
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
      INTEGER I,N,NQ
!S    REAL
      real(wp)                                                  &
         AUG,CONV,DEN,PSI,FOUR,FOURTH,HALF,ONE,P1,P2,PIOV4,Q1,Q2,       &
         SGN,THREE,XLARGE,UPPER,W,X,XINF,XMAX1,XMIN1,XSMALL,X01,        &
         X01D,X02,XX,Z,ZERO
      DIMENSION P1(9),P2(7),Q1(8),Q2(6)
!----------------------------------------------------------------------
!  Mathematical constants.  PIOV4 = pi / 4
!----------------------------------------------------------------------
!S    DATA ZERO,FOURTH,HALF,ONE/0.0E0,0.25E0,0.5E0,1.0E0/
!S    DATA THREE,FOUR/3.0E0,4.0E0/,PIOV4/7.8539816339744830962E-01/
      DATA ZERO,FOURTH,HALF,ONE/0.0D0,0.25D0,0.5D0,1.0D0/
      DATA THREE,FOUR/3.0D0,4.0D0/,PIOV4/7.8539816339744830962D-01/
!----------------------------------------------------------------------
!  Machine-dependent constants
!----------------------------------------------------------------------
!S    DATA XINF/1.70E+38/, XMIN1/5.89E-39/, XMAX1/8.38E+06/,
!S   1     XSMALL/1.35E-04/, XLARGE/1.20E+06/
      DATA XINF/1.70D+38/, XMIN1/5.89D-39/, XMAX1/3.60D+16/,            &
           XSMALL/2.05D-09/, XLARGE/2.04D+15/
!----------------------------------------------------------------------
!  Zero of psi(x)
!----------------------------------------------------------------------
!S    DATA X01/187.0E0/,X01D/128.0E0/,X02/6.9464496836234126266E-04/
      DATA X01/187.0D0/,X01D/128.0D0/,X02/6.9464496836234126266D-04/
!----------------------------------------------------------------------
!  Coefficients for approximation to  psi(x)/(x-x0)  over [0.5, 3.0]
!----------------------------------------------------------------------
!S    DATA P1/4.5104681245762934160E-03,5.4932855833000385356E+00,
!S   1        3.7646693175929276856E+02,7.9525490849151998065E+03,
!S   2        7.1451595818951933210E+04,3.0655976301987365674E+05,
!S   3        6.3606997788964458797E+05,5.8041312783537569993E+05,
!S   4        1.6585695029761022321E+05/
!S    DATA Q1/9.6141654774222358525E+01,2.6287715790581193330E+03,
!S   1        2.9862497022250277920E+04,1.6206566091533671639E+05,
!S   2        4.3487880712768329037E+05,5.4256384537269993733E+05,
!S   3        2.4242185002017985252E+05,6.4155223783576225996E-08/
      DATA P1/4.5104681245762934160D-03,5.4932855833000385356D+00,      &
              3.7646693175929276856D+02,7.9525490849151998065D+03,      &
              7.1451595818951933210D+04,3.0655976301987365674D+05,      &
              6.3606997788964458797D+05,5.8041312783537569993D+05,      &
              1.6585695029761022321D+05/
      DATA Q1/9.6141654774222358525D+01,2.6287715790581193330D+03,      &
              2.9862497022250277920D+04,1.6206566091533671639D+05,      &
              4.3487880712768329037D+05,5.4256384537269993733D+05,      &
              2.4242185002017985252D+05,6.4155223783576225996D-08/
!----------------------------------------------------------------------
!  Coefficients for approximation to  psi(x) - ln(x) + 1/(2x)
!     for  x > 3.0
!----------------------------------------------------------------------
!S    DATA P2/-2.7103228277757834192E+00,-1.5166271776896121383E+01,
!S   1        -1.9784554148719218667E+01,-8.8100958828312219821E+00,
!S   2        -1.4479614616899842986E+00,-7.3689600332394549911E-02,
!S   3        -6.5135387732718171306E-21/
!S    DATA Q2/ 4.4992760373789365846E+01, 2.0240955312679931159E+02,
!S   1         2.4736979003315290057E+02, 1.0742543875702278326E+02,
!S   2         1.7463965060678569906E+01, 8.8427520398873480342E-01/
      DATA P2/-2.7103228277757834192D+00,-1.5166271776896121383D+01,    &
              -1.9784554148719218667D+01,-8.8100958828312219821D+00,    &
              -1.4479614616899842986D+00,-7.3689600332394549911D-02,    &
              -6.5135387732718171306D-21/
      DATA Q2/ 4.4992760373789365846D+01, 2.0240955312679931159D+02,    &
               2.4736979003315290057D+02, 1.0742543875702278326D+02,    &
               1.7463965060678569906D+01, 8.8427520398873480342D-01/
!----------------------------------------------------------------------
!S    CONV(I) = REAL(I)
      CONV(I) = real(i,wp)
      X = XX
      W = ABS(X)
      AUG = ZERO
!----------------------------------------------------------------------
!  Check for valid arguments, then branch to appropriate algorithm
!----------------------------------------------------------------------
      IF ((-X .GE. XMAX1) .OR. (W .LT. XMIN1)) THEN
            GO TO 410
         ELSE IF (X .GE. HALF) THEN
            GO TO 200
!----------------------------------------------------------------------
!  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x)
!     Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL.
!----------------------------------------------------------------------
         ELSE IF (W .LE. XSMALL) THEN
            AUG = -ONE / X
            GO TO 150
      END IF
!----------------------------------------------------------------------
!  Argument reduction for cot
!----------------------------------------------------------------------
  100 IF (X .LT. ZERO) THEN
            SGN = PIOV4
         ELSE
            SGN = -PIOV4
      END IF
      W = W - AINT(W)
      NQ = INT(W * FOUR)
      W = FOUR * (W - CONV(NQ) * FOURTH)
!----------------------------------------------------------------------
!  W is now related to the fractional part of  4.0 * X.
!     Adjust argument to correspond to values in the first
!     quadrant and determine the sign.
!----------------------------------------------------------------------
      N = NQ / 2
      IF ((N+N) .NE. NQ) W = ONE - W
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
            IF (Z .EQ. ZERO) GO TO 410
            AUG = SGN * (FOUR / TAN(Z))
         ELSE
            AUG = SGN * (FOUR * TAN(Z))
      END IF
  150 X = ONE - X
  200 IF (X .GT. THREE) GO TO 300
!----------------------------------------------------------------------
!  0.5 <= X <= 3.0
!----------------------------------------------------------------------
      DEN = X
      UPPER = P1(1) * X
      DO 210 I = 1, 7
         DEN = (DEN + Q1(I)) * X
         UPPER = (UPPER + P1(I+1)) * X
  210 CONTINUE
      DEN = (UPPER + P1(9)) / (DEN + Q1(8))
      X = (X-X01/X01D) - X02
      PSI = DEN * X + AUG
      GO TO 500
!----------------------------------------------------------------------
!  3.0 < X
!----------------------------------------------------------------------
  300 IF (X .LT. XLARGE) THEN
         W = ONE / (X * X)
         DEN = W
         UPPER = P2(1) * W
         DO 310 I = 1, 5
            DEN = (DEN + Q2(I)) * W
            UPPER = (UPPER + P2(I+1)) * W
  310    CONTINUE
         AUG = (UPPER + P2(7)) / (DEN + Q2(6)) - HALF / X + AUG
      END IF
      PSI = AUG + LOG(X)
      GO TO 500
!----------------------------------------------------------------------
!  Error return
!----------------------------------------------------------------------
  410 PSI = XINF
      IF (X .GT. ZERO) PSI = -XINF
  500 RETURN
!---------- Last card of PSI ----------
      END function psi





!***********************************************************************************************************************************
!  CGAMMA
!
!  Complex gamma function.
!  Formulae from "An Atlas of Functions" by Spanier and Oldham, Sect. 43:11.
!***********************************************************************************************************************************

      complex(wp) FUNCTION CGAMMA (Z) RESULT (R)

      COMPLEX(wp), INTENT(IN) :: Z

      real(wp), PARAMETER :: EPS = 1.0D-14
      real(wp), PARAMETER :: PI = 4._wp * atan(1._wp)
      real(wp), PARAMETER :: EULER = 0.57721566490153286060651209008240243104215933593992359880576723488486772677766467094D0

      INTEGER :: J
      real(wp) :: X, Y, THETA, SUM, PROD, PSUM, PPROD


      X = DBLE(Z)
      Y = AIMAG(Z)

      IF (Z .EQ. (0.0D0,0.0D0)) THEN
         error stop '  CGAMMA Error.'
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

      PROD = 1._wp
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

complex(wp) FUNCTION CBETA (X,Y) RESULT (R)

COMPLEX(wp), INTENT(IN) :: X, Y


R = CGAMMA(X)*CGAMMA(Y)/CGAMMA(X+Y)

END FUNCTION CBETA

!***********************************************************************************************************************************
!  RBETA
!
!  Rational beta function.
!***********************************************************************************************************************************

elemental SUBROUTINE RBETA (X, Y, N, D)

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
!  DEC_TO_FRAC
!
!  Converts a decimal number to a fraction.
!  Algorithm from "An Atlas of Functions" by Spanier and Oldham, Springer-Verlag, 1987, pp. 665-667.
!***********************************************************************************************************************************

      SUBROUTINE DEC_TO_FRAC (X, NUM, DEN, TOL)

      IMPLICIT NONE

      real(wp), INTENT(IN) :: X
      INTEGER, INTENT(OUT) :: NUM, DEN
      real(wp), INTENT(IN), OPTIONAL :: TOL

      real(wp), PARAMETER :: TOL_DEF = 1.0D-6                               ! default value of tolerance
      real(wp) :: TOL1, NU, R, T, EPS, M
      INTEGER :: N1, N2, D1, D2
      LOGICAL :: SGN

!
!     Set a default value for TOL if TOL was not provided.
!

      IF (PRESENT(TOL)) THEN
         TOL1 = TOL
      ELSE
         TOL1 = TOL_DEF
      END IF

!
!     Save the sign of X, and make it positive.
!

      NU = X                                                                        ! make a local copy of X
      SGN = NU .LT. 0.0D0                                                           ! save sign
      NU = ABS(NU)                                                                  ! remove sign from X

!
!     Compute the rational equivalent of X.
!

      D1 = 1
      D2 = 1
      N1 = INT(NU)
      N2 = N1 + 1
      GO TO 300
  100 IF (R .GT. 1._wp) GO TO 200
      R = 1._wp/R
  200 N2 = N2 + N1*INT(R)
      D2 = D2 + D1*INT(R)
      N1 = N1 + N2
      D1 = D1 + D2
  300 R = 0.0D0
      IF (NU*D1 .EQ. DBLE(N1)) GO TO 400
      R = (N2-NU*D2)/(NU*D1-N1)
      IF (R .GT. 1._wp) GO TO 400
      T = N2
      N2 = N1
      N1 = T
      T = D2
      D2 = D1
      D1 = T
  400 EPS = ABS(1._wp - (N1/(NU*D1)))
      IF (EPS .LE. TOL1) GO TO 600
      M = 1._wp
  500 M = 10*M
      IF (M*EPS .LT. 1._wp) GO TO 500
      EPS = (1._wp/M)*INT(0.5D0+M*EPS)
  600 IF (EPS .LE. TOL1) THEN
         NUM = N1
         DEN = D1
         IF (SGN) NUM = -NUM                                                        ! negate numerator if needed
         RETURN
      END IF
      IF (R .NE. 0.0D0) GO TO 100

      RETURN

      END SUBROUTINE DEC_TO_FRAC
!***********************************************************************************************************************************
!  H2HMSD
!
!  Convert decimal hours to hours, minutes, and seconds.  Seconds are returned as a double-precision value.
!***********************************************************************************************************************************

      SUBROUTINE H2HMSD (DHR, IHR, IMIN, SEC)

      IMPLICIT NONE

      real(wp), INTENT(IN) :: DHR
      INTEGER, INTENT(OUT) :: IHR, IMIN
      real(wp), INTENT(OUT) :: SEC
      real(wp) :: TIME


      TIME = DHR                                                                    ! hours
      IHR = INT(TIME)                                                               ! hours
      TIME = 60.0D0 * (TIME - IHR)                                                  ! minutes
      IMIN = INT(TIME)                                                              ! minutes
      SEC = 60.0D0 * (TIME - IMIN)                                                  ! seconds

      RETURN

      END SUBROUTINE H2HMSD





!***********************************************************************************************************************************
!  HMS2H
!
!  Convert hours, minutes, and seconds to decimal hours.
!***********************************************************************************************************************************

      elemental SUBROUTINE HMS2H (IHR, IMIN, SEC, DHR)

      INTEGER, INTENT(IN) :: IHR, IMIN
      real(wp), INTENT(IN) :: SEC
      real(wp), INTENT(OUT) :: DHR


      DHR = DBLE(IHR) + DBLE(IMIN)/60.0D0 + SEC/3600.0D0

      END SUBROUTINE HMS2H





!***********************************************************************************************************************************
!  ISFRAC
!
!  Returns .TRUE. if X has a fractional part (i.e. if X is not an integer)
!***********************************************************************************************************************************

elemental logical FUNCTION ISFRAC (X) RESULT (Y)
real(wp), INTENT(IN) :: X
real(wp), PARAMETER :: EPS = 1.0D-8

y = (ABS(X)-INT(ABS(X))) > EPS
END FUNCTION ISFRAC


!***********************************************************************************************************************************
!  ISINT
!
!  Returns .TRUE. if X has no fractional part (i.e. if X is an integer)
!***********************************************************************************************************************************

elemental logical FUNCTION ISINT (X)
real(wp), INTENT(IN) :: X

isint = (ABS(X)-INT(ABS(X))) < epsilon(0._wp)
END FUNCTION ISINT


!***********************************************************************************************************************************
!  FRAC_TO_MIXED
!
!  Convert a fraction from improper format to mixed format.
!***********************************************************************************************************************************

elemental SUBROUTINE FRAC_TO_MIXED (AN, AD, A1, A2, A3)

INTEGER, INTENT(IN) :: AN, AD
INTEGER, INTENT(OUT) :: A1, A2, A3
INTEGER :: ANN, ADN
LOGICAL :: NEGFLAG

ANN = AN                                                                      ! normalize the input fraction
ADN = AD
CALL RATNORM (ANN,ADN)

NEGFLAG = ANN .LT. 0                                                          ! save the sign of the fraction..
ANN = ABS (ANN)                                                               ! ..and take its absolute value

A1 = ANN / ADN                                                                ! find components of mixed fraction
A2 = ANN - A1*ADN
A3 = ADN

IF (NEGFLAG) A1 = -A1                                                         ! restore the sign (assign to A1)

END SUBROUTINE FRAC_TO_MIXED


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

NSTERM = S*(S+1.0D00)*(S+2.0D00)* &
  (S+3.0D00)*(S+4.0D00)/30240.0D00
N = (NSTERM*(2.0D00**S)/EPS) &
    **(1._wp/(S+5.0D00))
IF ( N < 10 )  THEN
   N = 10
END IF

FN = N
NEGS = -S
!     Direct sum
SUM = 0.0D00
DO K =2, N-1
   SUM = SUM+K**NEGS
END DO

!     Add Euler-Maclaurin correction terms
SUM = SUM+(FN**NEGS)*(0.5D00+FN/(S-1.0D00) &
  +S*(1._wp-(S+ 1._wp)*(S+2.0D00)/ &
  (60.0D00*FN*FN)) &
  /(12.0D00*FN))+NSTERM/(FN**(S+5.0D00))
riemannZETA = SUM

END FUNCTION RIEMANNZETA





!***********************************************************************************************************************************
!  LINREG
!
!  Real linear regression.
!***********************************************************************************************************************************

elemental SUBROUTINE LINREG (M, B, R)

USE GLOBAL

real(wp), INTENT(OUT) :: M, B, R

M = (NN*SUMXY-SUMX*SUMY)/(NN*SUMX2-SUMX**2)
B = (SUMY*SUMX2-SUMX*SUMXY)/(NN*SUMX2-SUMX**2)
R = (SUMXY-SUMX*SUMY/NN)/SQRT((SUMX2-SUMX**2/NN)*(SUMY2-SUMY**2/NN))


END SUBROUTINE LINREG

!***********************************************************************************************************************************
!  CLINREG
!
!  Complex linear regression.
!***********************************************************************************************************************************

elemental SUBROUTINE CLINREG (M, B, R)

USE GLOBAL, only: cnn, csumxy, csumx, csumx2, csumy, csumy2

COMPLEX(wp), INTENT(OUT) :: M, B, R

M = (CNN*CSUMXY-CSUMX*CSUMY)/(CNN*CSUMX2-CSUMX**2)
B = (CSUMY*CSUMX2-CSUMX*CSUMXY)/(CNN*CSUMX2-CSUMX**2)
R = (CSUMXY-CSUMX*CSUMY/CNN)/SQRT((CSUMX2-CSUMX**2/CNN)*(CSUMY2-CSUMY**2/CNN))

END SUBROUTINE CLINREG
!***********************************************************************************************************************************
!  RLINREG
!
!  Rational linear regression.
!***********************************************************************************************************************************

elemental SUBROUTINE RLINREG (NM, DM, NB, DB, R)

USE GLOBAL

INTEGER, INTENT(OUT) :: NM, DM, NB, DB
real(wp), INTENT(OUT) :: R
INTEGER :: NUM, DEN, NUM2, DEN2, NUM3, DEN3, NUM4, DEN4
real(wp) :: DNN, DSUMX, DSUMX2, DSUMY, DSUMY2, DSUMXY


CALL RMUL(RNNN,RDNN,RNSUMXY,RDSUMXY,NUM,DEN)
CALL RMUL(RNSUMX,RDSUMX,RNSUMY,RDSUMY,NUM2,DEN2)
CALL RSUB(NUM,DEN,NUM2,DEN2,NUM3,DEN3)
CALL RMUL(RNNN,RDNN,RNSUMX2,RDSUMX2,NUM,DEN)
CALL RMUL(RNSUMX,RDSUMX,RNSUMX,RDSUMX,NUM2,DEN2)
CALL RSUB(NUM,DEN,NUM2,DEN2,NUM4,DEN4)

CALL RDIV(NUM3,DEN3,NUM4,DEN4,NM,DM)

CALL RMUL(RNSUMY,RDSUMY,RNSUMX2,RDSUMX2,NUM,DEN)
CALL RMUL(RNSUMX,RDSUMX,RNSUMXY,RDSUMXY,NUM2,DEN2)
CALL RSUB(NUM,DEN,NUM2,DEN2,NUM3,DEN3)
CALL RMUL(RNNN,RDNN,RNSUMX2,RDSUMX2,NUM,DEN)
CALL RMUL(RNSUMX,RDSUMX,RNSUMX,RDSUMX,NUM2,DEN2)
CALL RSUB(NUM,DEN,NUM2,DEN2,NUM4,DEN4)

CALL RDIV(NUM3,DEN3,NUM4,DEN4,NB,DB)

DNN = DBLE(RNNN)/DBLE(RDNN)
DSUMX = DBLE(RNSUMX)/DBLE(RDSUMX)
DSUMX2 = DBLE(RNSUMX2)/DBLE(RDSUMX2)
DSUMY = DBLE(RNSUMY)/DBLE(RDSUMY)
DSUMY2 = DBLE(RNSUMY2)/DBLE(RDSUMY2)
DSUMXY = DBLE(RNSUMXY)/DBLE(RDSUMXY)

R = (DSUMXY-DSUMX*DSUMY/DNN)/SQRT((DSUMX2-DSUMX**2/DNN)*(DSUMY2-DSUMY**2/DNN))

END SUBROUTINE RLINREG


!***********************************************************************************************************************************
!  REDUCE
!
!  Reduce an angle to the range [angle_min, angle_max).
!***********************************************************************************************************************************

      elemental real(wp) FUNCTION REDUCE (THETA, ANGLE_MIN) RESULT (RHO)

      real(wp), PARAMETER :: TWOPI = 2*4._wp*atan(1._wp)

      real(wp), INTENT(IN) :: THETA
      real(wp), INTENT(IN) :: ANGLE_MIN

      real(wp) :: ANGLE_MAX
      real(wp) :: REVS


!
!     Start of code.
!

      ANGLE_MAX = ANGLE_MIN + TWOPI

      IF (THETA .LT. ANGLE_MIN) THEN
         REVS = AINT((ANGLE_MIN-THETA)/TWOPI) + 1
         RHO = THETA + REVS*TWOPI
      ELSE IF (THETA .GE. ANGLE_MAX) THEN
         REVS = AINT((THETA-ANGLE_MIN)/TWOPI)
         RHO = THETA - REVS*TWOPI
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
!     TWOPI        = 2*PI
!     PI_SQR       = PI**2
!     THREE_PI_SQR = 3 * PI**2
!     ONEP6_PI     = 1.6*PI
!     TWO_THIRDS   = 2/3
!     SIXTH        = 1/6
!     R24          = 1/24
!

      real(wp), PARAMETER :: PI = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803D0
      real(wp), PARAMETER :: TWOPI = 6.28318530717958647692528676655900576839433879875021164194988918461563281257241799726D0
      real(wp), PARAMETER :: PI_SQR = 9.8696044010893586188344909998761511353136994072407906264133493762200448224192052430D0
      real(wp), PARAMETER :: THREE_PI_SQR = 29.608813203268075856503472999628453405941098221722371879240048128660134467258D0
      real(wp), PARAMETER :: ONEP6_PI = 5.02654824574366918154022941324720461471547103900016931355991134769250625005793440D0
      real(wp), PARAMETER :: TWO_THIRDS = 0.666666666666666666666666666666666666666666666666666666666666666666666666666667D0
      real(wp), PARAMETER :: SIXTH = 0.16666666666666666666666666666666666666666666666666666666666666666666666666666666667D0
      real(wp), PARAMETER :: R24 = 4.166666666666666666666666666666666666666666666666666666666666666666666666666666666667D-2

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

      ALPHA = (THREE_PI_SQR + ONEP6_PI*(PI-ABS(M))/(1._wp+ECC)) / (PI_SQR - 6.0D0)
      D = 3.0D0*(1._wp-ECC) + ALPHA*ECC
      Q = 2.0D0*ALPHA*D*(1._wp-ECC)-M**2
      R = 3.0D0*ALPHA*D*(D-1.0D0+ECC)*M + M**3
      W = (ABS(R) + SQRT(Q**3 + R**2)) ** TWO_THIRDS

!
!     Compute first-order solution to Kepler's Equation (E1).
!

      E1 = (2.0D0*R*W/(W**2 + W*Q + Q**2) + M) / D

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
      DELTA4 = -F/(F1+0.5D0*DELTA3*F2+SIXTH*DELTA3**2*F3)
      DELTA5 = -F/(F1+0.5D0*DELTA4*F2+SIXTH*DELTA4**2*F3+R24*DELTA4**3*F4)

!
!     Find fifth-order refined estimate of E (E5).
!

      E5 = E1 + DELTA5

!
!     Put E5 in the range [0, 2*PI) and return.
!

      E5 = REDUCE (E5, 0._wp)

      END FUNCTION KEPLER


!***********************************************************************************************************************************
!  RJBESL
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE RJBESL(X, ALPHA, NB, B, NCALC)
!---------------------------------------------------------------------
! This routine calculates Bessel functions J sub(N+ALPHA) (X)
!   for non-negative argument X, and non-negative order N+ALPHA.
!
!
!  Explanation of variables in the calling sequence.
!
!   X     - working precision non-negative real argument for which
!           J's are to be calculated.
!   ALPHA - working precision fractional part of order for which
!           J's or exponentially scaled J'r (J*exp(X)) are
!           to be calculated.  0 <= ALPHA < 1.0.
!   NB  - integer number of functions to be calculated, NB > 0.
!           The first function calculated is of order ALPHA, and the
!           last is of order (NB - 1 + ALPHA).
!   B  - working precision output vector of length NB.  If RJBESL
!           terminates normally (NCALC=NB), the vector B contains the
!           functions J/ALPHA/(X) through J/NB-1+ALPHA/(X), or the
!           corresponding exponentially scaled functions.
!   NCALC - integer output variable indicating possible errors.
!           Before using the vector B, the user should check that
!           NCALC=NB, i.e., all orders have been calculated to
!           the desired accuracy.  See Error Returns below.
!
!
!*******************************************************************
!*******************************************************************
!
!  Explanation of machine-dependent constants
!
!   it     = Number of bits in the mantissa of a working precision
!            variable
!   NSIG   = Decimal significance desired.  Should be set to
!            INT(LOG10(2)*it+1).  Setting NSIG lower will result
!            in decreased accuracy while setting NSIG higher will
!            increase CPU time without increasing accuracy.  The
!            truncation error is limited to a relative error of
!            T=.5*10**(-NSIG).
!   ENTEN  = 10.0 ** K, where K is the largest integer such that
!            ENTEN is machine-representable in working precision
!   ENSIG  = 10.0 ** NSIG
!   RTNSIG = 10.0 ** (-K) for the smallest integer K such that
!            K .GE. NSIG/4
!   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
!   XLARGE = Upper limit on the magnitude of X.  If ABS(X)=N,
!            then at least N iterations of the backward recursion
!            will be executed.  The value of 10.0 ** 4 is used on
!            every machine.
!
!
!     Approximate values for some important machines are:
!
!
!                            it    NSIG    ENTEN       ENSIG
!
!   CRAY-1        (S.P.)     48     15    1.0E+2465   1.0E+15
!   Cyber 180/855
!     under NOS   (S.P.)     48     15    1.0E+322    1.0E+15
!   IEEE (IBM/XT,
!     SUN, etc.)  (S.P.)     24      8    1.0E+38     1.0E+8
!   IEEE (IBM/XT,
!     SUN, etc.)  (D.P.)     53     16    1.0D+308    1.0D+16
!   IBM 3033      (D.P.)     14      5    1.0D+75     1.0D+5
!   VAX           (S.P.)     24      8    1.0E+38     1.0E+8
!   VAX D-Format  (D.P.)     56     17    1.0D+38     1.0D+17
!   VAX G-Format  (D.P.)     53     16    1.0D+307    1.0D+16
!
!
!                           RTNSIG      ENMTEN      XLARGE
!
!   CRAY-1        (S.P.)    1.0E-4    1.84E-2466   1.0E+4
!   Cyber 180/855
!     under NOS   (S.P.)    1.0E-4    1.25E-293    1.0E+4
!   IEEE (IBM/XT,
!     SUN, etc.)  (S.P.)    1.0E-2    4.70E-38     1.0E+4
!   IEEE (IBM/XT,
!     SUN, etc.)  (D.P.)    1.0E-4    8.90D-308    1.0D+4
!   IBM 3033      (D.P.)    1.0E-2    2.16D-78     1.0D+4
!   VAX           (S.P.)    1.0E-2    1.17E-38     1.0E+4
!   VAX D-Format  (D.P.)    1.0E-5    1.17D-38     1.0D+4
!   VAX G-Format  (D.P.)    1.0E-4    2.22D-308    1.0D+4
!
!*******************************************************************
!*******************************************************************
!
!  Error returns
!
!    In case of an error,  NCALC .NE. NB, and not all J's are
!    calculated to the desired accuracy.
!
!    NCALC .LT. 0:  An argument is out of range. For example,
!       NBES .LE. 0, ALPHA .LT. 0 or .GT. 1, or X is too large.
!       In this case, B(1) is set to zero, the remainder of the
!       B-vector is not calculated, and NCALC is set to
!       MIN(NB,0)-1 so that NCALC .NE. NB.
!
!    NB .GT. NCALC .GT. 0: Not all requested function values could
!       be calculated accurately.  This usually occurs because NB is
!       much larger than ABS(X).  In this case, B(N) is calculated
!       to the desired accuracy for N .LE. NCALC, but precision
!       is lost for NCALC .LT. N .LE. NB.  If B(N) does not vanish
!       for N .GT. NCALC (because it is too small to be represented),
!       and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
!       significant figures of B(N) can be trusted.
!
!
!  Intrinsic and other functions required are:
!
!     ABS, AINT, COS, DBLE, GAMMA, INT, MAX, MIN,
!
!     REAL, SIN, SQRT
!
!
!  Acknowledgement
!
!   This program is based on a program written by David J. Sookne
!   (2) that computes values of the Bessel functions J or I of real
!   argument and integer order.  Modifications include the restriction
!   of the computation to the J Bessel function of non-negative real
!   argument, the extension of the computation to arbitrary positive
!   order, and the elimination of most underflow.
!
!  References: "A Note on Backward Recurrence Algorithms," Olver,
!               F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
!               pp 941-947.
!
!              "Bessel Functions of Real Argument and Integer Order,"
!               Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
!               125-132.
!
!  Latest modification: March 19, 1990
!
!  Author: W. J. Cody
!          Applied Mathematics Division
!          Argonne National Laboratory
!          Argonne, IL  60439
!
!---------------------------------------------------------------------
      INTEGER I,J,K,L,M,MAGX,N,NB,NBMX,NCALC,NEND,NSTART

      real(wp) :: ALPHA,ALPEM,ALP2EM,B,CAPP,CAPQ,CONV,EIGHTH,EM,EN,ENMTEN,ENSIG,   &
       FACT,FOUR,FUNC,GNU,HALF,HALFX,ONE,ONE30,P,PI2,PLAST,       &
       POLD,PSAVE,PSAVEL,RTNSIG,S,SUM,T,T1,TEMPA,TEMPB,TEMPC,TEST,      &
       THREE,THREE5,TOVER,TWO,TWOFIV,TWOPI1,TWOPI2,X,XC,XIN,XK,XLARGE,  &
       XM,VCOS,VSIN,Z,ZERO
      DIMENSION B(NB), FACT(25)
!---------------------------------------------------------------------
!  Mathematical constants
!
!   PI2    - 2 / PI
!   TWOPI1 - first few significant digits of 2 * PI
!   TWOPI2 - (2*PI - TWOPI) to working precision, i.e.,
!            TWOPI1 + TWOPI2 = 2 * PI to extra precision.
!---------------------------------------------------------------------
!S    DATA PI2, TWOPI1, TWOPI2 /0.636619772367581343075535E0,6.28125E0,
!S   1 1.935307179586476925286767E-3/
!S    DATA ZERO, EIGHTH, HALF, ONE /0.0E0,0.125E0,0.5E0,1.0E0/
!S    DATA TWO, THREE, FOUR, TWOFIV /2.0E0,3.0E0,4.0E0,25.0E0/
!S    DATA ONE30, THREE5 /130.0E0,35.0E0/
      DATA PI2, TWOPI1, TWOPI2 /0.636619772367581343075535D0,6.28125D0, &
       1.935307179586476925286767D-3/
      DATA ZERO, EIGHTH, HALF, ONE /0.0D0,0.125D0,0.5D0,1.0D0/
      DATA TWO, THREE, FOUR, TWOFIV /2.0D0,3.0D0,4.0D0,25.0D0/
      DATA ONE30, THREE5 /130.0D0,35.0D0/
!---------------------------------------------------------------------
!  Machine-dependent parameters
!---------------------------------------------------------------------
!S    DATA ENTEN, ENSIG, RTNSIG /1.0E38,1.0E8,1.0E-2/
!S    DATA ENMTEN, XLARGE /1.2E-37,1.0E4/
      DATA ENSIG, RTNSIG /1.0D17,1.0D-4/
      DATA ENMTEN, XLARGE /1.2D-37,1.0D4/
!---------------------------------------------------------------------
!     Factorial(N)
!---------------------------------------------------------------------
!S    DATA FACT /1.0E0,1.0E0,2.0E0,6.0E0,24.0E0,1.2E2,7.2E2,5.04E3,
!S   1 4.032E4,3.6288E5,3.6288E6,3.99168E7,4.790016E8,6.2270208E9,
!S   2 8.71782912E10,1.307674368E12,2.0922789888E13,3.55687428096E14,
!S   3 6.402373705728E15,1.21645100408832E17,2.43290200817664E18,
!S   4 5.109094217170944E19,1.12400072777760768E21,
!S   5 2.585201673888497664E22,6.2044840173323943936E23/
      DATA FACT /1.0D0,1.0D0,2.0D0,6.0D0,24.0D0,1.2D2,7.2D2,5.04D3,     &
       4.032D4,3.6288D5,3.6288D6,3.99168D7,4.790016D8,6.2270208D9,      &
       8.71782912D10,1.307674368D12,2.0922789888D13,3.55687428096D14,   &
       6.402373705728D15,1.21645100408832D17,2.43290200817664D18,       &
       5.109094217170944D19,1.12400072777760768D21,                     &
       2.585201673888497664D22,6.2044840173323943936D23/
!---------------------------------------------------------------------
! Statement functions for conversion and the gamma function.
!---------------------------------------------------------------------
!S    CONV(I) = REAL(I)
!S    FUNC(X) = GAMMA(X)
      CONV(I) = real(i,wp)
      FUNC(X) = gamma(X)
!---------------------------------------------------------------------
! Check for out of range arguments.
!---------------------------------------------------------------------
      MAGX = INT(X)
      IF ((NB.GT.0) .AND. (X.GE.ZERO) .AND. (X.LE.XLARGE)               &
             .AND. (ALPHA.GE.ZERO) .AND. (ALPHA.LT.ONE))                &
         THEN
!---------------------------------------------------------------------
! Initialize result array to zero.
!---------------------------------------------------------------------
            NCALC = NB
            DO 20 I=1,NB
              B(I) = ZERO
   20       CONTINUE
!---------------------------------------------------------------------
! Branch to use 2-term ascending series for small X and asymptotic
! form for large X when NB is not too large.
!---------------------------------------------------------------------
            IF (X.LT.RTNSIG) THEN
!---------------------------------------------------------------------
! Two-term ascending series for small X.
!---------------------------------------------------------------------
               TEMPA = ONE
               ALPEM = ONE + ALPHA
               HALFX = ZERO
               IF (X.GT.ENMTEN) HALFX = HALF*X
               IF (ALPHA.NE.ZERO)                                       &
                  TEMPA = HALFX**ALPHA/(ALPHA*FUNC(ALPHA))
               TEMPB = ZERO
               IF ((X+ONE).GT.ONE) TEMPB = -HALFX*HALFX
               B(1) = TEMPA + TEMPA*TEMPB/ALPEM
               IF ((X.NE.ZERO) .AND. (B(1).EQ.ZERO)) NCALC = 0
               IF (NB .NE. 1) THEN
                  IF (X .LE. ZERO) THEN
                        DO 30 N=2,NB
                          B(N) = ZERO
   30                   CONTINUE
                     ELSE
!---------------------------------------------------------------------
! Calculate higher order functions.
!---------------------------------------------------------------------
                        TEMPC = HALFX
                        TOVER = (ENMTEN+ENMTEN)/X
                        IF (TEMPB.NE.ZERO) TOVER = ENMTEN/TEMPB
                        DO 50 N=2,NB
                          TEMPA = TEMPA/ALPEM
                          ALPEM = ALPEM + ONE
                          TEMPA = TEMPA*TEMPC
                          IF (TEMPA.LE.TOVER*ALPEM) TEMPA = ZERO
                          B(N) = TEMPA + TEMPA*TEMPB/ALPEM
                          IF ((B(N).EQ.ZERO) .AND. (NCALC.GT.N))        &
                             NCALC = N-1
   50                   CONTINUE
                  END IF
               END IF
            ELSE IF ((X.GT.TWOFIV) .AND. (NB.LE.MAGX+1)) THEN
!---------------------------------------------------------------------
! Asymptotic series for X .GT. 21.0.
!---------------------------------------------------------------------
               XC = SQRT(PI2/X)
               XIN = (EIGHTH/X)**2
               M = 11
               IF (X.GE.THREE5) M = 8
               IF (X.GE.ONE30) M = 4
               XM = FOUR*CONV(M)
!---------------------------------------------------------------------
! Argument reduction for SIN and COS routines.
!---------------------------------------------------------------------
               T = AINT(X/(TWOPI1+TWOPI2)+HALF)
               Z = ((X-T*TWOPI1)-T*TWOPI2) - (ALPHA+HALF)/PI2
               VSIN = SIN(Z)
               VCOS = COS(Z)
               GNU = ALPHA + ALPHA
               DO 80 I=1,2
                 S = ((XM-ONE)-GNU)*((XM-ONE)+GNU)*XIN*HALF
                 T = (GNU-(XM-THREE))*(GNU+(XM-THREE))
                 CAPP = S*T/FACT(2*M+1)
                 T1 = (GNU-(XM+ONE))*(GNU+(XM+ONE))
                 CAPQ = S*T1/FACT(2*M+2)
                 XK = XM
                 K = M + M
                 T1 = T
                 DO 70 J=2,M
                   XK = XK - FOUR
                   S = ((XK-ONE)-GNU)*((XK-ONE)+GNU)
                   T = (GNU-(XK-THREE))*(GNU+(XK-THREE))
                   CAPP = (CAPP+ONE/FACT(K-1))*S*T*XIN
                   CAPQ = (CAPQ+ONE/FACT(K))*S*T1*XIN
                   K = K - 2
                   T1 = T
   70            CONTINUE
                 CAPP = CAPP + ONE
                 CAPQ = (CAPQ+ONE)*(GNU*GNU-ONE)*(EIGHTH/X)
                 B(I) = XC*(CAPP*VCOS-CAPQ*VSIN)
                 IF (NB.EQ.1) GO TO 300
                 T = VSIN
                 VSIN = -VCOS
                 VCOS = T
                 GNU = GNU + TWO
   80         CONTINUE
!---------------------------------------------------------------------
! If  NB .GT. 2, compute J(X,ORDER+I)  I = 2, NB-1
!---------------------------------------------------------------------
               IF (NB .GT. 2) THEN
                  GNU = ALPHA + ALPHA + TWO
                  DO 90 J=3,NB
                    B(J) = GNU*B(J-1)/X - B(J-2)
                    GNU = GNU + TWO
   90             CONTINUE
               END IF
!---------------------------------------------------------------------
! Use recurrence to generate results.  First initialize the
! calculation of P*S.
!---------------------------------------------------------------------
            ELSE
               NBMX = NB - MAGX
               N = MAGX + 1
               EN = CONV(N+N) + (ALPHA+ALPHA)
               PLAST = ONE
               P = EN/X
!---------------------------------------------------------------------
! Calculate general significance test.
!---------------------------------------------------------------------
               TEST = ENSIG + ENSIG
               IF (NBMX .GE. 3) THEN
!---------------------------------------------------------------------
! Calculate P*S until N = NB-1.  Check for possible overflow.
!---------------------------------------------------------------------
                  TOVER = ENTEN/ENSIG
                  NSTART = MAGX + 2
                  NEND = NB - 1
                  EN = CONV(NSTART+NSTART) - TWO + (ALPHA+ALPHA)
                  DO 130 K=NSTART,NEND
                     N = K
                     EN = EN + TWO
                     POLD = PLAST
                     PLAST = P
                     P = EN*PLAST/X - POLD
                     IF (P.GT.TOVER) THEN
!---------------------------------------------------------------------
! To avoid overflow, divide P*S by TOVER.  Calculate P*S until
! ABS(P) .GT. 1.
!---------------------------------------------------------------------
                        TOVER = ENTEN
                        P = P/TOVER
                        PLAST = PLAST/TOVER
                        PSAVE = P
                        PSAVEL = PLAST
                        NSTART = N + 1
  100                   N = N + 1
                           EN = EN + TWO
                           POLD = PLAST
                           PLAST = P
                           P = EN*PLAST/X - POLD
                        IF (P.LE.ONE) GO TO 100
                        TEMPB = EN/X
!---------------------------------------------------------------------
! Calculate backward test and find NCALC, the highest N such that
! the test is passed.
!---------------------------------------------------------------------
                        TEST = POLD*PLAST*(HALF-HALF/(TEMPB*TEMPB))
                        TEST = TEST/ENSIG
                        P = PLAST*TOVER
                        N = N - 1
                        EN = EN - TWO
                        NEND = MIN(NB,N)
                        DO 110 L=NSTART,NEND
                           POLD = PSAVEL
                           PSAVEL = PSAVE
                           PSAVE = EN*PSAVEL/X - POLD
                           IF (PSAVE*PSAVEL.GT.TEST) THEN
                              NCALC = L - 1
                              GO TO 190
                           END IF
  110                   CONTINUE
                        NCALC = NEND
                        GO TO 190
                     END IF
  130             CONTINUE
                  N = NEND
                  EN = CONV(N+N) + (ALPHA+ALPHA)
!---------------------------------------------------------------------
! Calculate special significance test for NBMX .GT. 2.
!---------------------------------------------------------------------
                  TEST = MAX(TEST,SQRT(PLAST*ENSIG)*SQRT(P+P))
               END IF
!---------------------------------------------------------------------
! Calculate P*S until significance test passes.
!---------------------------------------------------------------------
  140          N = N + 1
                  EN = EN + TWO
                  POLD = PLAST
                  PLAST = P
                  P = EN*PLAST/X - POLD
               IF (P.LT.TEST) GO TO 140
!---------------------------------------------------------------------
! Initialize the backward recursion and the normalization sum.
!---------------------------------------------------------------------
  190          N = N + 1
               EN = EN + TWO
               TEMPB = ZERO
               TEMPA = ONE/P
               M = 2*N - 4*(N/2)
               SUM = ZERO
               EM = CONV(N/2)
               ALPEM = (EM-ONE) + ALPHA
               ALP2EM = (EM+EM) + ALPHA
               IF (M .NE. 0) SUM = TEMPA*ALPEM*ALP2EM/EM
               NEND = N - NB
               IF (NEND .GT. 0) THEN
!---------------------------------------------------------------------
! Recur backward via difference equation, calculating (but not
! storing) B(N), until N = NB.
!---------------------------------------------------------------------
                  DO 200 L=1,NEND
                     N = N - 1
                     EN = EN - TWO
                     TEMPC = TEMPB
                     TEMPB = TEMPA
                     TEMPA = (EN*TEMPB)/X - TEMPC
                     M = 2 - M
                     IF (M .NE. 0) THEN
                        EM = EM - ONE
                        ALP2EM = (EM+EM) + ALPHA
                        IF (N.EQ.1) GO TO 210
                        ALPEM = (EM-ONE) + ALPHA
                        IF (ALPEM.EQ.ZERO) ALPEM = ONE
                        SUM = (SUM+TEMPA*ALP2EM)*ALPEM/EM
                     END IF
  200             CONTINUE
               END IF
!---------------------------------------------------------------------
! Store B(NB).
!---------------------------------------------------------------------
  210          B(N) = TEMPA
               IF (NEND .GE. 0) THEN
                  IF (NB .LE. 1) THEN
                        ALP2EM = ALPHA
                        IF ((ALPHA+ONE).EQ.ONE) ALP2EM = ONE
                        SUM = SUM + B(1)*ALP2EM
                        GO TO 250
                     ELSE
!---------------------------------------------------------------------
! Calculate and store B(NB-1).
!---------------------------------------------------------------------
                        N = N - 1
                        EN = EN - TWO
                        B(N) = (EN*TEMPA)/X - TEMPB
                        IF (N.EQ.1) GO TO 240
                        M = 2 - M
                        IF (M .NE. 0) THEN
                           EM = EM - ONE
                           ALP2EM = (EM+EM) + ALPHA
                           ALPEM = (EM-ONE) + ALPHA
                           IF (ALPEM.EQ.ZERO) ALPEM = ONE
                           SUM = (SUM+B(N)*ALP2EM)*ALPEM/EM
                        END IF
                  END IF
               END IF
               NEND = N - 2
               IF (NEND .NE. 0) THEN
!---------------------------------------------------------------------
! Calculate via difference equation and store B(N), until N = 2.
!---------------------------------------------------------------------
                  DO 230 L=1,NEND
                     N = N - 1
                     EN = EN - TWO
                     B(N) = (EN*B(N+1))/X - B(N+2)
                     M = 2 - M
                     IF (M .NE. 0) THEN
                        EM = EM - ONE
                        ALP2EM = (EM+EM) + ALPHA
                        ALPEM = (EM-ONE) + ALPHA
                        IF (ALPEM.EQ.ZERO) ALPEM = ONE
                        SUM = (SUM+B(N)*ALP2EM)*ALPEM/EM
                     END IF
  230             CONTINUE
               END IF
!---------------------------------------------------------------------
! Calculate B(1).
!---------------------------------------------------------------------
               B(1) = TWO*(ALPHA+ONE)*B(2)/X - B(3)
  240          EM = EM - ONE
               ALP2EM = (EM+EM) + ALPHA
               IF (ALP2EM.EQ.ZERO) ALP2EM = ONE
               SUM = SUM + B(1)*ALP2EM
!---------------------------------------------------------------------
! Normalize.  Divide all B(N) by sum.
!---------------------------------------------------------------------
  250          IF ((ALPHA+ONE).NE.ONE)                                  &
                    SUM = SUM*FUNC(ALPHA)*(X*HALF)**(-ALPHA)
               TEMPA = ENMTEN
               IF (SUM.GT.ONE) TEMPA = TEMPA*SUM
               DO 260 N=1,NB
                 IF (ABS(B(N)).LT.TEMPA) B(N) = ZERO
                 B(N) = B(N)/SUM
  260          CONTINUE
            END IF
!---------------------------------------------------------------------
! Error return -- X, NB, or ALPHA is out of range.
!---------------------------------------------------------------------
         ELSE
            B(1) = ZERO
            NCALC = MIN(NB,0) - 1
      END IF
!---------------------------------------------------------------------
! Exit
!---------------------------------------------------------------------
  300 RETURN
! ---------- Last line of RJBESL ----------
      END





!***********************************************************************************************************************************
!  RYBESL
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE RYBESL(X,ALPHA,NB,BY,NCALC)
!----------------------------------------------------------------------
!
!  This routine calculates Bessel functions Y SUB(N+ALPHA) (X)
!  for non-negative argument X, and non-negative order N+ALPHA.
!
!
! Explanation of variables in the calling sequence
!
! X     - Working precision non-negative real argument for which
!         Y's are to be calculated.
! ALPHA - Working precision fractional part of order for which
!         Y's are to be calculated.  0 .LE. ALPHA .LT. 1.0.
! NB    - Integer number of functions to be calculated, NB .GT. 0.
!         The first function calculated is of order ALPHA, and the
!         last is of order (NB - 1 + ALPHA).
! BY    - Working precision output vector of length NB.  If the
!         routine terminates normally (NCALC=NB), the vector BY
!         contains the functions Y(ALPHA,X), ... , Y(NB-1+ALPHA,X),
!         If (0 .LT. NCALC .LT. NB), BY(I) contains correct function
!         values for I .LE. NCALC, and contains the ratios
!         Y(ALPHA+I-1,X)/Y(ALPHA+I-2,X) for the rest of the array.
! NCALC - Integer output variable indicating possible errors.
!         Before using the vector BY, the user should check that
!         NCALC=NB, i.e., all orders have been calculated to
!         the desired accuracy.  See error returns below.
!
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   p      = Number of significant base-beta digits in the
!            significand of a floating-point number
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   EPS    = beta ** (-p)
!   DEL    = Machine number below which sin(x)/x = 1; approximately
!            SQRT(EPS).
!   XMIN   = Smallest acceptable argument for RBESY; approximately
!            max(2*beta**minexp,2/XINF), rounded up
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   THRESH = Lower bound for use of the asymptotic form; approximately
!            AINT(-LOG10(EPS/2.0))+1.0
!   XLARGE = Upper bound on X; approximately 1/DEL, because the sine
!            and cosine functions have lost about half of their
!            precision at that point.
!
!
!     Approximate values for some important machines are:
!
!                        beta    p     minexp      maxexp      EPS
!
!  CRAY-1        (S.P.)    2    48     -8193        8191    3.55E-15
!  Cyber 180/185
!    under NOS   (S.P.)    2    48      -975        1070    3.55E-15
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)    2    24      -126         128    5.96E-8
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)    2    53     -1022        1024    1.11D-16
!  IBM 3033      (D.P.)   16    14       -65          63    1.39D-17
!  VAX           (S.P.)    2    24      -128         127    5.96E-8
!  VAX D-Format  (D.P.)    2    56      -128         127    1.39D-17
!  VAX G-Format  (D.P.)    2    53     -1024        1023    1.11D-16
!
!
!                         DEL      XMIN      XINF     THRESH  XLARGE
!
! CRAY-1        (S.P.)  5.0E-8  3.67E-2466 5.45E+2465  15.0E0  2.0E7
! Cyber 180/855
!   under NOS   (S.P.)  5.0E-8  6.28E-294  1.26E+322   15.0E0  2.0E7
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)  1.0E-4  2.36E-38   3.40E+38     8.0E0  1.0E4
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)  1.0D-8  4.46D-308  1.79D+308   16.0D0  1.0D8
! IBM 3033      (D.P.)  1.0D-8  2.77D-76   7.23D+75    17.0D0  1.0D8
! VAX           (S.P.)  1.0E-4  1.18E-38   1.70E+38     8.0E0  1.0E4
! VAX D-Format  (D.P.)  1.0D-9  1.18D-38   1.70D+38    17.0D0  1.0D9
! VAX G-Format  (D.P.)  1.0D-8  2.23D-308  8.98D+307   16.0D0  1.0D8
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  In case of an error, NCALC .NE. NB, and not all Y's are
!  calculated to the desired accuracy.
!
!  NCALC .LT. -1:  An argument is out of range. For example,
!       NB .LE. 0, IZE is not 1 or 2, or IZE=1 and ABS(X) .GE.
!       XMAX.  In this case, BY(1) = 0.0, the remainder of the
!       BY-vector is not calculated, and NCALC is set to
!       MIN0(NB,0)-2  so that NCALC .NE. NB.
!  NCALC = -1:  Y(ALPHA,X) .GE. XINF.  The requested function
!       values are set to 0.0.
!  1 .LT. NCALC .LT. NB: Not all requested function values could
!       be calculated accurately.  BY(I) contains correct function
!       values for I .LE. NCALC, and and the remaining NB-NCALC
!       array elements contain 0.0.
!
!
! Intrinsic functions required are:
!
!     DBLE, EXP, INT, MAX, MIN, REAL, SQRT
!
!
! Acknowledgement
!
!  This program draws heavily on Temme's Algol program for Y(a,x)
!  and Y(a+1,x) and on Campbell's programs for Y_nu(x).  Temme's
!  scheme is used for  x < THRESH, and Campbell's scheme is used
!  in the asymptotic region.  Segments of code from both sources
!  have been translated into Fortran 77, merged, and heavily modified.
!  Modifications include parameterization of machine dependencies,
!  use of a new approximation for ln(gamma(x)), and built-in
!  protection against over/underflow.
!
! References: "Bessel functions J_nu(x) and Y_nu(x) of real
!              order and real argument," Campbell, J. B.,
!              Comp. Phy. Comm. 18, 1979, pp. 133-142.
!
!             "On the numerical evaluation of the ordinary
!              Bessel function of the second kind," Temme,
!              N. M., J. Comput. Phys. 21, 1976, pp. 343-350.
!
!  Latest modification: March 19, 1990
!
!  Modified by: W. J. Cody
!               Applied Mathematics Division
!               Argonne National Laboratory
!               Argonne, IL  60439
!
!----------------------------------------------------------------------
      INTEGER I,K,NA,NB,NCALC
!S    REAL
      real(wp)                                                  &
        ALFA,ALPHA,AYE,B,BY,C,CH,COSMU,D,DEL,DEN,DDIV,DIV,DMU,D1,D2,    &
        E,EIGHT,EN,ENU,EN1,EPS,EVEN,EX,F,FIVPI,G,GAMMA,H,HALF,ODD,      &
        ONBPI,ONE,ONE5,P,PA,PA1,PI,PIBY2,PIM5,Q,QA,QA1,Q0,R,S,SINMU,    &
        SQ2BPI,TEN9,TERM,THREE,THRESH,TWO,TWOBYX,X,XINF,XLARGE,    &
        XNA,X2,YA,YA1,ZERO
      DIMENSION BY(NB),CH(21)
!----------------------------------------------------------------------
!  Mathematical constants
!    FIVPI = 5*PI
!    PIM5 = 5*PI - 15
!    ONBPI = 1/PI
!    PIBY2 = PI/2
!    SQ2BPI = SQUARE ROOT OF 2/PI
!----------------------------------------------------------------------
!S    DATA ZERO,HALF,ONE,TWO,THREE/0.0E0,0.5E0,1.0E0,2.0E0,3.0E0/
!S    DATA EIGHT,ONE5,TEN9/8.0E0,15.0E0,1.9E1/
!S    DATA FIVPI,PIBY2/1.5707963267948966192E1,1.5707963267948966192E0/
!S    DATA PI,SQ2BPI/3.1415926535897932385E0,7.9788456080286535588E-1/
!S    DATA PIM5,ONBPI/7.0796326794896619231E-1,3.1830988618379067154E-1/
      DATA ZERO,HALF,ONE,TWO,THREE/0.0D0,0.5D0,1.0D0,2.0D0,3.0D0/
      DATA EIGHT,ONE5,TEN9/8.0D0,15.0D0,1.9D1/
      DATA FIVPI,PIBY2/1.5707963267948966192D1,1.5707963267948966192D0/
      DATA PI,SQ2BPI/3.1415926535897932385D0,7.9788456080286535588D-1/
      DATA PIM5,ONBPI/7.0796326794896619231D-1,3.1830988618379067154D-1/
!----------------------------------------------------------------------
!  Machine-dependent constants
!----------------------------------------------------------------------
!S    DATA DEL,XMIN,XINF,EPS/1.0E-4,2.36E-38,3.40E38,5.96E-8/
!S    DATA THRESH,XLARGE/8.0E0,1.0E4/
      DATA DEL,EPS/1.0D-8,1.11D-16/
      DATA THRESH,XLARGE/16.0D0,1.0D8/
!----------------------------------------------------------------------
!  Coefficients for Chebyshev polynomial expansion of
!         1/gamma(1-x), abs(x) .le. .5
!----------------------------------------------------------------------
!S    DATA CH/-0.67735241822398840964E-23,-0.61455180116049879894E-22,
!S   1         0.29017595056104745456E-20, 0.13639417919073099464E-18,
!S   2         0.23826220476859635824E-17,-0.90642907957550702534E-17,
!S   3        -0.14943667065169001769E-14,-0.33919078305362211264E-13,
!S   4        -0.17023776642512729175E-12, 0.91609750938768647911E-11,
!S   5         0.24230957900482704055E-09, 0.17451364971382984243E-08,
!S   6        -0.33126119768180852711E-07,-0.86592079961391259661E-06,
!S   7        -0.49717367041957398581E-05, 0.76309597585908126618E-04,
!S   8         0.12719271366545622927E-02, 0.17063050710955562222E-02,
!S   9        -0.76852840844786673690E-01,-0.28387654227602353814E+00,
!S   A         0.92187029365045265648E+00/
      DATA CH/-0.67735241822398840964D-23,-0.61455180116049879894D-22,  &
               0.29017595056104745456D-20, 0.13639417919073099464D-18,  &
               0.23826220476859635824D-17,-0.90642907957550702534D-17,  &
              -0.14943667065169001769D-14,-0.33919078305362211264D-13,  &
              -0.17023776642512729175D-12, 0.91609750938768647911D-11,  &
               0.24230957900482704055D-09, 0.17451364971382984243D-08,  &
              -0.33126119768180852711D-07,-0.86592079961391259661D-06,  &
              -0.49717367041957398581D-05, 0.76309597585908126618D-04,  &
               0.12719271366545622927D-02, 0.17063050710955562222D-02,  &
              -0.76852840844786673690D-01,-0.28387654227602353814D+00,  &
               0.92187029365045265648D+00/
!----------------------------------------------------------------------
      EX = X
      ENU = ALPHA
      IF ((NB .GT. 0) .AND. (X .GE. XMIN) .AND. (EX .LT. XLARGE)        &
             .AND. (ENU .GE. ZERO) .AND. (ENU .LT. ONE))  THEN
            XNA = AINT(ENU+HALF)
            NA = INT(XNA)
            IF (NA .EQ. 1) ENU = ENU - XNA
            IF (ENU .EQ. -HALF) THEN
                  P = SQ2BPI/SQRT(EX)
                  YA = P * SIN(EX)
                  YA1 = -P * COS(EX)
               ELSE IF (EX .LT. THREE) THEN
!----------------------------------------------------------------------
!  Use Temme's scheme for small X
!----------------------------------------------------------------------
                  B = EX * HALF
                  D = -LOG(B)
                  F = ENU * D
                  E = B**(-ENU)
                  IF (ABS(ENU) .LT. DEL) THEN
                        C = ONBPI
                     ELSE
                        C = ENU / SIN(ENU*PI)
                  END IF
!----------------------------------------------------------------------
!  Computation of sinh(f)/f
!----------------------------------------------------------------------
                  IF (ABS(F) .LT. ONE) THEN
                        X2 = F*F
                        EN = TEN9
                        S = ONE
                        DO 80 I = 1, 9
                           S = S*X2/EN/(EN-ONE)+ONE
                           EN = EN - TWO
   80                   CONTINUE
                     ELSE
                        S = (E - ONE/E) * HALF / F
                  END IF
!----------------------------------------------------------------------
!  Computation of 1/gamma(1-a) using Chebyshev polynomials
!----------------------------------------------------------------------
                  X2 = ENU*ENU*EIGHT
                  AYE = CH(1)
                  EVEN = ZERO
                  ALFA = CH(2)
                  ODD = ZERO
                  DO 40 I = 3, 19, 2
                     EVEN = -(AYE+AYE+EVEN)
                     AYE = -EVEN*X2 - AYE + CH(I)
                     ODD = -(ALFA+ALFA+ODD)
                     ALFA = -ODD*X2 - ALFA + CH(I+1)
   40             CONTINUE
                  EVEN = (EVEN*HALF+AYE)*X2 - AYE + CH(21)
                  ODD = (ODD+ALFA)*TWO
                  GAMMA = ODD*ENU + EVEN
!----------------------------------------------------------------------
!  End of computation of 1/gamma(1-a)
!----------------------------------------------------------------------
                  G = E * GAMMA
                  E = (E + ONE/E) * HALF
                  F = TWO*C*(ODD*E+EVEN*S*D)
                  E = ENU*ENU
                  P = G*C
                  Q = ONBPI / G
                  C = ENU*PIBY2
                  IF (ABS(C) .LT. DEL) THEN
                        R = ONE
                     ELSE
                        R = SIN(C)/C
                  END IF
                  R = PI*C*R*R
                  C = ONE
                  D = - B*B
                  H = ZERO
                  YA = F + R*Q
                  YA1 = P
                  EN = ZERO
  100             EN = EN + ONE
                  IF (ABS(G/(ONE+ABS(YA)))                              &
                            + ABS(H/(ONE+ABS(YA1))) .GT. EPS) THEN
                        F = (F*EN+P+Q)/(EN*EN-E)
                        C = C * D/EN
                        P = P/(EN-ENU)
                        Q = Q/(EN+ENU)
                        G = C*(F+R*Q)
                        H = C*P - EN*G
                        YA = YA + G
                        YA1 = YA1+H
                        GO TO 100
                  END IF
                  YA = -YA
                  YA1 = -YA1/B
               ELSE IF (EX .LT. THRESH) THEN
!----------------------------------------------------------------------
!  Use Temme's scheme for moderate X
!----------------------------------------------------------------------
                  C = (HALF-ENU)*(HALF+ENU)
                  B = EX + EX
                  E = (EX*ONBPI*COS(ENU*PI)/EPS)
                  E = E*E
                  P = ONE
                  Q = -EX
                  R = ONE + EX*EX
                  S = R
                  EN = TWO
  200             IF (R*EN*EN .LT. E) THEN
                        EN1 = EN+ONE
                        D = (EN-ONE+C/EN)/S
                        P = (EN+EN-P*D)/EN1
                        Q = (-B+Q*D)/EN1
                        S = P*P + Q*Q
                        R = R*S
                        EN = EN1
                        GO TO 200
                  END IF
                  F = P/S
                  P = F
                  G = -Q/S
                  Q = G
  220             EN = EN - ONE
                  IF (EN .GT. ZERO) THEN
                        R = EN1*(TWO-P)-TWO
                        S = B + EN1*Q
                        D = (EN-ONE+C/EN)/(R*R+S*S)
                        P = D*R
                        Q = D*S
                        E = F + ONE
                        F = P*E - G*Q
                        G = Q*E + P*G
                        EN1 = EN
                        GO TO 220
                  END IF
                  F = ONE + F
                  D = F*F + G*G
                  PA = F/D
                  QA = -G/D
                  D = ENU + HALF -P
                  Q = Q + EX
                  PA1 = (PA*Q-QA*D)/EX
                  QA1 = (QA*Q+PA*D)/EX
                  B = EX - PIBY2*(ENU+HALF)
                  C = COS(B)
                  S = SIN(B)
                  D = SQ2BPI/SQRT(EX)
                  YA = D*(PA*S+QA*C)
                  YA1 = D*(QA1*S-PA1*C)
               ELSE
!----------------------------------------------------------------------
!  Use Campbell's asymptotic scheme.
!----------------------------------------------------------------------
                  NA = 0
                  D1 = AINT(EX/FIVPI)
                  I = INT(D1)
                  DMU = ((EX-ONE5*D1)-D1*PIM5)-(ALPHA+HALF)*PIBY2
                  IF (I-2*(I/2) .EQ. 0) THEN
                        COSMU = COS(DMU)
                        SINMU = SIN(DMU)
                     ELSE
                        COSMU = -COS(DMU)
                        SINMU = -SIN(DMU)
                  END IF
                  DDIV = EIGHT * EX
                  DMU = ALPHA
                  DEN = SQRT(EX)
                  DO 350 K = 1, 2
                     P = COSMU
                     COSMU = SINMU
                     SINMU = -P
                     D1 = (TWO*DMU-ONE)*(TWO*DMU+ONE)
                     D2 = ZERO
                     DIV = DDIV
                     P = ZERO
                     Q = ZERO
                     Q0 = D1/DIV
                     TERM = Q0
                     DO 310 I = 2, 20
                        D2 = D2 + EIGHT
                        D1 = D1 - D2
                        DIV = DIV + DDIV
                        TERM = -TERM*D1/DIV
                        P = P + TERM
                        D2 = D2 + EIGHT
                        D1 = D1 - D2
                        DIV = DIV + DDIV
                        TERM = TERM*D1/DIV
                        Q = Q + TERM
                        IF (ABS(TERM) .LE. EPS) GO TO 320
  310                CONTINUE
  320                P = P + ONE
                     Q = Q + Q0
                     IF (K .EQ. 1) THEN
                           YA = SQ2BPI * (P*COSMU-Q*SINMU) / DEN
                        ELSE
                           YA1 = SQ2BPI * (P*COSMU-Q*SINMU) / DEN
                     END IF
                     DMU = DMU + ONE
  350             CONTINUE
            END IF
            IF (NA .EQ. 1) THEN
               H = TWO*(ENU+ONE)/EX
               IF (H .GT. ONE) THEN
                  IF (ABS(YA1) .GT. XINF/H) THEN
                     H = ZERO
                     YA = ZERO
                  END IF
               END IF
               H = H*YA1 - YA
               YA = YA1
               YA1 = H
            END IF
!----------------------------------------------------------------------
!  Now have first one or two Y's
!----------------------------------------------------------------------
            BY(1) = YA
            BY(2) = YA1
            IF (YA1 .EQ. ZERO) THEN
                  NCALC = 1
               ELSE
                  AYE = ONE + ALPHA
                  TWOBYX = TWO/EX
                  NCALC = 2
                  DO 400 I = 3, NB
                     IF (TWOBYX .LT. ONE) THEN
                           IF (ABS(BY(I-1))*TWOBYX .GE. XINF/AYE)       &
                                                           GO TO 450
                        ELSE
                           IF (ABS(BY(I-1)) .GE. XINF/AYE/TWOBYX )      &
                                                           GO TO 450
                     END IF
                     BY(I) = TWOBYX*AYE*BY(I-1) - BY(I-2)
                     AYE = AYE + ONE
                     NCALC = NCALC + 1
  400             CONTINUE
            END IF
  450       DO 460 I = NCALC+1, NB
               BY(I) = ZERO
  460       CONTINUE
         ELSE
            BY(1) = ZERO
            NCALC = MIN(NB,0) - 1
      END IF
  900 RETURN
!---------- Last line of RYBESL ----------
      END





!***********************************************************************************************************************************
!  CALCI0
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE CALCI0(ARG,RESULT,JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functions of the first kind
!   and order zero, I0(X) and EXP(-ABS(X))*I0(X), for real
!   arguments X.  It contains two function type subprograms, BESI0
!   and BESEI0, and one subroutine type subprogram, CALCI0.
!   The calling statements for the primary entries are
!
!                   Y=BESI0(X)
!   and
!                   Y=BESEI0(X)
!
!   where the entry points correspond to the functions I0(X) and
!   EXP(-ABS(X))*I0(X), respectively.  The routine CALCI0 is
!   intended for internal packet use only, all computations within
!   the packet being concentrated in this routine.  The function
!   subprograms invoke CALCI0 with the statement
!          CALL CALCI0(ARG,RESULT,JINT)
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALCI0
!       Call              ARG                  RESULT          JINT
!
!     BESI0(ARG)    ABS(ARG) .LE. XMAX        I0(ARG)           1
!     BESEI0(ARG)    any real ARG        EXP(-ABS(ARG))*I0(ARG) 2
!
!   The main computation evaluates slightly modified forms of
!   minimax approximations generated by Blair and Edwards, Chalk
!   River (Atomic Energy of Canada Limited) Report AECL-4928,
!   October, 1974.  This transportable program is patterned after
!   the machine-dependent FUNPACK packet NATSI0, but cannot match
!   that version for efficiency or accuracy.  This version uses
!   rational functions that theoretically approximate I-SUB-0(X)
!   to at least 18 significant decimal digits.  The accuracy
!   achieved depends on the arithmetic system, the compiler, the
!   intrinsic functions, and proper selection of the machine-
!   dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   maxexp = Smallest power of beta that overflows
!   XSMALL = Positive argument such that 1.0 - X = 1.0 to
!            machine precision for all ABS(X) .LE. XSMALL.
!   XINF =   Largest positive machine number; approximately
!            beta**maxexp
!   XMAX =   Largest argument acceptable to BESI0;  Solution to
!            equation:
!               W(X) * (1+1/(8*X)+9/(128*X**2) = beta**maxexp
!            where  W(X) = EXP(X)/SQRT(2*PI*X)
!
!
!     Approximate values for some important machines are:
!
!                          beta       maxexp       XSMALL
!
! CRAY-1        (S.P.)       2         8191       3.55E-15
! Cyber 180/855
!   under NOS   (S.P.)       2         1070       3.55E-15
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       2          128       2.98E-8
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       2         1024       5.55D-17
! IBM 3033      (D.P.)      16           63       6.95D-18
! VAX           (S.P.)       2          127       2.98E-8
! VAX D-Format  (D.P.)       2          127       6.95D-18
! VAX G-Format  (D.P.)       2         1023       5.55D-17
!
!
!                               XINF          XMAX
!
! CRAY-1        (S.P.)       5.45E+2465     5682.810
! Cyber 180/855
!   under NOS   (S.P.)       1.26E+322       745.893
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       3.40E+38         91.900
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       1.79D+308       713.986
! IBM 3033      (D.P.)       7.23D+75        178.182
! VAX           (S.P.)       1.70D+38         91.203
! VAX D-Format  (D.P.)       1.70D+38         91.203
! VAX G-Format  (D.P.)       8.98D+307       713.293
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns XINF for BESI0 for ABS(ARG) .GT. XMAX.
!
!
!  Intrinsic functions required are:
!
!     ABS, SQRT, EXP
!
!
!  Authors: W. J. Cody and L. Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL 60439
!
!  Latest modification: June 7, 1988
!
!--------------------------------------------------------------------
      INTEGER I,JINT
!S    REAL
      real(wp)                                                  &
             A,ARG,B,EXP40,FORTY,ONE,ONE5,P,PP,Q,QQ,RESULT,             &
             REC15,SUMP,SUMQ,TWO25,X,XINF,XMAX,XSMALL,XX
      DIMENSION P(15),PP(8),Q(5),QQ(7)
!--------------------------------------------------------------------
!  Mathematical constants
!--------------------------------------------------------------------
!S    DATA ONE/1.0E0/,ONE5/15.0E0/,EXP40/2.353852668370199854E17/,
!S   1     FORTY/40.0E0/,REC15/6.6666666666666666666E-2/,
!S   2     TWO25/225.0E0/
      DATA ONE/1.0D0/,ONE5/15.0D0/,EXP40/2.353852668370199854D17/,      &
           FORTY/40.0D0/,REC15/6.6666666666666666666D-2/,               &
           TWO25/225.0D0/
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
!S    DATA XSMALL/2.98E-8/,XINF/3.40E38/,XMAX/91.9E0/
      DATA XSMALL/5.55D-17/,XMAX/713.986_wp/
!--------------------------------------------------------------------
!  Coefficients for XSMALL .LE. ABS(ARG) .LT. 15.0
!--------------------------------------------------------------------
!S    DATA  P/-5.2487866627945699800E-18,-1.5982226675653184646E-14,
!S   1        -2.6843448573468483278E-11,-3.0517226450451067446E-08,
!S   2        -2.5172644670688975051E-05,-1.5453977791786851041E-02,
!S   3        -7.0935347449210549190E+00,-2.4125195876041896775E+03,
!S   4        -5.9545626019847898221E+05,-1.0313066708737980747E+08,
!S   5        -1.1912746104985237192E+10,-8.4925101247114157499E+11,
!S   6        -3.2940087627407749166E+13,-5.5050369673018427753E+14,
!S   7        -2.2335582639474375249E+15/
!S    DATA  Q/-3.7277560179962773046E+03, 6.5158506418655165707E+06,
!S   1        -6.5626560740833869295E+09, 3.7604188704092954661E+12,
!S   2        -9.7087946179594019126E+14/
      DATA  P/-5.2487866627945699800D-18,-1.5982226675653184646D-14,    &
              -2.6843448573468483278D-11,-3.0517226450451067446D-08,    &
              -2.5172644670688975051D-05,-1.5453977791786851041D-02,    &
              -7.0935347449210549190D+00,-2.4125195876041896775D+03,    &
              -5.9545626019847898221D+05,-1.0313066708737980747D+08,    &
              -1.1912746104985237192D+10,-8.4925101247114157499D+11,    &
              -3.2940087627407749166D+13,-5.5050369673018427753D+14,    &
              -2.2335582639474375249D+15/
      DATA  Q/-3.7277560179962773046D+03, 6.5158506418655165707D+06,    &
              -6.5626560740833869295D+09, 3.7604188704092954661D+12,    &
              -9.7087946179594019126D+14/
!--------------------------------------------------------------------
!  Coefficients for 15.0 .LE. ABS(ARG)
!--------------------------------------------------------------------
!S    DATA PP/-3.9843750000000000000E-01, 2.9205384596336793945E+00,
!S   1        -2.4708469169133954315E+00, 4.7914889422856814203E-01,
!S   2        -3.7384991926068969150E-03,-2.6801520353328635310E-03,
!S   3         9.9168777670983678974E-05,-2.1877128189032726730E-06/
!S    DATA QQ/-3.1446690275135491500E+01, 8.5539563258012929600E+01,
!S   1        -6.0228002066743340583E+01, 1.3982595353892851542E+01,
!S   2        -1.1151759188741312645E+00, 3.2547697594819615062E-02,
!S   3        -5.5194330231005480228E-04/
      DATA PP/-3.9843750000000000000D-01, 2.9205384596336793945D+00,    &
              -2.4708469169133954315D+00, 4.7914889422856814203D-01,    &
              -3.7384991926068969150D-03,-2.6801520353328635310D-03,    &
               9.9168777670983678974D-05,-2.1877128189032726730D-06/
      DATA QQ/-3.1446690275135491500D+01, 8.5539563258012929600D+01,    &
              -6.0228002066743340583D+01, 1.3982595353892851542D+01,    &
              -1.1151759188741312645D+00, 3.2547697594819615062D-02,    &
              -5.5194330231005480228D-04/
!--------------------------------------------------------------------
      X = ABS(ARG)
      IF (X .LT. XSMALL) THEN
            RESULT = ONE
         ELSE IF (X .LT. ONE5) THEN
!--------------------------------------------------------------------
!  XSMALL .LE.  ABS(ARG)  .LT. 15.0
!--------------------------------------------------------------------
            XX = X * X
            SUMP = P(1)
            DO 50 I = 2, 15
              SUMP = SUMP * XX + P(I)
   50       CONTINUE
            XX = XX - TWO25
            SUMQ = ((((XX+Q(1))*XX+Q(2))*XX+Q(3))*XX+Q(4))*XX+Q(5)
            RESULT = SUMP / SUMQ
            IF (JINT .EQ. 2) RESULT = RESULT * EXP(-X)
         ELSE IF (X .GE. ONE5) THEN
            IF ((JINT .EQ. 1) .AND. (X .GT. XMAX)) THEN
                  RESULT = XINF
               ELSE
!--------------------------------------------------------------------
!  15.0  .LE.  ABS(ARG)
!--------------------------------------------------------------------
                  XX = ONE / X - REC15
                  SUMP = ((((((PP(1)*XX+PP(2))*XX+PP(3))*XX+PP(4))*XX+  &
                         PP(5))*XX+PP(6))*XX+PP(7))*XX+PP(8)
                  SUMQ = ((((((XX+QQ(1))*XX+QQ(2))*XX+QQ(3))*XX+        &
                         QQ(4))*XX+QQ(5))*XX+QQ(6))*XX+QQ(7)
                  RESULT = SUMP / SUMQ
                  IF (JINT .EQ. 2) THEN
                        RESULT = (RESULT - PP(1)) / SQRT(X)
                     ELSE
!--------------------------------------------------------------------
!  Calculation reformulated to avoid premature overflow
!--------------------------------------------------------------------
                        IF (X .LE.(XMAX-ONE5)) THEN
                              A = EXP(X)
                              B = ONE
                           ELSE
                              A = EXP(X-FORTY)
                              B = EXP40
                        END IF
                        RESULT = ((RESULT*A-PP(1)*A)/SQRT(X))*B
                  END IF
            END IF
      END IF
!--------------------------------------------------------------------
!  Return for ABS(ARG) .LT. XSMALL
!--------------------------------------------------------------------
      RETURN
!----------- Last line of CALCI0 -----------
      END





!***********************************************************************************************************************************
!  BESI0
!***********************************************************************************************************************************

!S    REAL
      real(wp)                                                  &
          FUNCTION BESI0(X)
!--------------------------------------------------------------------
!
! This long precision subprogram computes approximate values for
!   modified Bessel functions of the first kind of order zero for
!   arguments ABS(ARG) .LE. XMAX  (see comments heading CALCI0).
!
!--------------------------------------------------------------------
      INTEGER JINT
!S    REAL
      real(wp)                                                  &
          X, RESULT
!--------------------------------------------------------------------
      JINT=1
      CALL CALCI0(X,RESULT,JINT)
      BESI0=RESULT
      RETURN
!---------- Last line of BESI0 ----------
      END





!***********************************************************************************************************************************
!  BESEI0
!***********************************************************************************************************************************

!S    REAL
      real(wp)                                                  &
          FUNCTION BESEI0(X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the first kind of order zero
!   multiplied by EXP(-ABS(X)), where EXP is the
!   exponential function, ABS is the absolute value, and X
!   is any argument.
!
!--------------------------------------------------------------------
      INTEGER JINT
!S    REAL
      real(wp)                                                  &
          X, RESULT
!--------------------------------------------------------------------
      JINT=2
      CALL CALCI0(X,RESULT,JINT)
      BESEI0=RESULT
      RETURN
!---------- Last line of BESEI0 ----------
      END





!***********************************************************************************************************************************
!  CALCI1
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE CALCI1(ARG,RESULT,JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functioons of the first kind
!   and order one, I1(X) and EXP(-ABS(X))*I1(X), for real
!   arguments X.  It contains two function type subprograms, BESI1
!   and BESEI1, and one subroutine type subprogram, CALCI1.
!   The calling statements for the primary entries are
!
!                   Y=BESI1(X)
!   and
!                   Y=BESEI1(X)
!
!   where the entry points correspond to the functions I1(X) and
!   EXP(-ABS(X))*I1(X), respectively.  The routine CALCI1 is
!   intended for internal packet use only, all computations within
!   the packet being concentrated in this routine.  The function
!   subprograms invoke CALCI1 with the statement
!          CALL CALCI1(ARG,RESULT,JINT)
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALCI1
!       Call              ARG                  RESULT          JINT
!
!     BESI1(ARG)    ABS(ARG) .LE. XMAX        I1(ARG)           1
!     BESEI1(ARG)    any real ARG        EXP(-ABS(ARG))*I1(ARG) 2
!
!   The main computation evaluates slightly modified forms of
!   minimax approximations generated by Blair and Edwards, Chalk
!   River (Atomic Energy of Canada Limited) Report AECL-4928,
!   October, 1974.  This transportable program is patterned after
!   the machine-dependent FUNPACK packet NATSI1, but cannot match
!   that version for efficiency or accuracy.  This version uses
!   rational functions that theoretically approximate I-SUB-1(X)
!   to at least 18 significant decimal digits.  The accuracy
!   achieved depends on the arithmetic system, the compiler, the
!   intrinsic functions, and proper selection of the machine-
!   dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   maxexp = Smallest power of beta that overflows
!   XSMALL = Positive argument such that 1.0 - X = 1.0 to
!            machine precision for all ABS(X) .LE. XSMALL.
!   XINF =   Largest positive machine number; approximately
!            beta**maxexp
!   XMAX =   Largest argument acceptable to BESI1;  Solution to
!            equation:
!               EXP(X) * (1-3/(8*X)) / SQRT(2*PI*X) = beta**maxexp
!
!
!     Approximate values for some important machines are:
!
!                          beta       maxexp       XSMALL
!
! CRAY-1        (S.P.)       2         8191       3.55E-15
! Cyber 180/855
!   under NOS   (S.P.)       2         1070       3.55E-15
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       2          128       2.98E-8
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       2         1024       5.55D-17
! IBM 3033      (D.P.)      16           63       6.95D-18
! VAX           (S.P.)       2          127       2.98E-8
! VAX D-Format  (D.P.)       2          127       6.95D-18
! VAX G-Format  (D.P.)       2         1023       5.55D-17
!
!
!                               XINF          XMAX
!
! CRAY-1        (S.P.)       5.45E+2465     5682.810
! Cyber 180/855
!   under NOS   (S.P.)       1.26E+322       745.894
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       3.40E+38         91.906
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       1.79D+308       713.987
! IBM 3033      (D.P.)       7.23D+75        178.185
! VAX           (S.P.)       1.70D+38         91.209
! VAX D-Format  (D.P.)       1.70D+38         91.209
! VAX G-Format  (D.P.)       8.98D+307       713.293
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns the value XINF for ABS(ARG) .GT. XMAX.
!
!
! Intrinsic functions required are:
!
!     ABS, SQRT, EXP
!
!
!  Authors: W. J. Cody and L. Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL  60439
!
!  Latest modification: May 31, 1989
!
!--------------------------------------------------------------------
      INTEGER J,JINT
!S    REAL
      real(wp) A,ARG,B,EXP40,FORTY,HALF,ONE,ONE5,P,PBAR,PP,Q,QQ,REC15,       &
          RESULT,SUMP,SUMQ,TWO25,X,XINF,XMAX,XSMALL,XX,ZERO
      DIMENSION P(15),PP(8),Q(5),QQ(6)
!--------------------------------------------------------------------
!  Mathematical constants
!--------------------------------------------------------------------
!S    DATA ONE/1.0E0/,ONE5/15.0E0/,EXP40/2.353852668370199854E17/,
!S   1     FORTY/40.0E0/,REC15/6.6666666666666666666E-2/,
!S   2     TWO25/225.0E0/,HALF/0.5E0/,ZERO/0.0E0/
      DATA ONE/1.0D0/,ONE5/15.0D0/,EXP40/2.353852668370199854D17/,      &
           FORTY/40.0D0/,REC15/6.6666666666666666666D-2/,               &
           TWO25/225.0D0/,HALF/0.5D0/,ZERO/0.0D0/
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
!S    DATA XSMALL/2.98E-8/,XINF/3.4E38/,XMAX/91.906E0/
      DATA XSMALL/5.55D-17/,XMAX/713.987_wp/
!--------------------------------------------------------------------
!  Coefficients for XSMALL .LE. ABS(ARG) .LT. 15.0
!--------------------------------------------------------------------
!S    DATA P/-1.9705291802535139930E-19,-6.5245515583151902910E-16,
!S   1       -1.1928788903603238754E-12,-1.4831904935994647675E-09,
!S   2       -1.3466829827635152875E-06,-9.1746443287817501309E-04,
!S   3       -4.7207090827310162436E-01,-1.8225946631657315931E+02,
!S   4       -5.1894091982308017540E+04,-1.0588550724769347106E+07,
!S   5       -1.4828267606612366099E+09,-1.3357437682275493024E+11,
!S   6       -6.9876779648010090070E+12,-1.7732037840791591320E+14,
!S   7       -1.4577180278143463643E+15/
      DATA P/-1.9705291802535139930D-19,-6.5245515583151902910D-16,     &
             -1.1928788903603238754D-12,-1.4831904935994647675D-09,     &
             -1.3466829827635152875D-06,-9.1746443287817501309D-04,     &
             -4.7207090827310162436D-01,-1.8225946631657315931D+02,     &
             -5.1894091982308017540D+04,-1.0588550724769347106D+07,     &
             -1.4828267606612366099D+09,-1.3357437682275493024D+11,     &
             -6.9876779648010090070D+12,-1.7732037840791591320D+14,     &
             -1.4577180278143463643D+15/
!S    DATA Q/-4.0076864679904189921E+03, 7.4810580356655069138E+06,
!S   1       -8.0059518998619764991E+09, 4.8544714258273622913E+12,
!S   2       -1.3218168307321442305E+15/
      DATA Q/-4.0076864679904189921D+03, 7.4810580356655069138D+06,     &
             -8.0059518998619764991D+09, 4.8544714258273622913D+12,     &
             -1.3218168307321442305D+15/
!--------------------------------------------------------------------
!  Coefficients for 15.0 .LE. ABS(ARG)
!--------------------------------------------------------------------
!S    DATA PP/-6.0437159056137600000E-02, 4.5748122901933459000E-01,
!S   1        -4.2843766903304806403E-01, 9.7356000150886612134E-02,
!S   2        -3.2457723974465568321E-03,-3.6395264712121795296E-04,
!S   3         1.6258661867440836395E-05,-3.6347578404608223492E-07/
      DATA PP/-6.0437159056137600000D-02, 4.5748122901933459000D-01,    &
              -4.2843766903304806403D-01, 9.7356000150886612134D-02,    &
              -3.2457723974465568321D-03,-3.6395264712121795296D-04,    &
               1.6258661867440836395D-05,-3.6347578404608223492D-07/
!S    DATA QQ/-3.8806586721556593450E+00, 3.2593714889036996297E+00,
!S   1        -8.5017476463217924408E-01, 7.4212010813186530069E-02,
!S   2        -2.2835624489492512649E-03, 3.7510433111922824643E-05/
      DATA QQ/-3.8806586721556593450D+00, 3.2593714889036996297D+00,    &
              -8.5017476463217924408D-01, 7.4212010813186530069D-02,    &
              -2.2835624489492512649D-03, 3.7510433111922824643D-05/
!S    DATA PBAR/3.98437500E-01/
      DATA PBAR/3.98437500D-01/
!--------------------------------------------------------------------
      X = ABS(ARG)
      IF (X .LT. XSMALL) THEN
!--------------------------------------------------------------------
!  Return for ABS(ARG) .LT. XSMALL
!--------------------------------------------------------------------
            RESULT = HALF * X
         ELSE IF (X .LT. ONE5) THEN
!--------------------------------------------------------------------
!  XSMALL .LE. ABS(ARG) .LT. 15.0
!--------------------------------------------------------------------
            XX = X * X
            SUMP = P(1)
            DO 50 J = 2, 15
               SUMP = SUMP * XX + P(J)
   50          CONTINUE
            XX = XX - TWO25
            SUMQ = ((((XX+Q(1))*XX+Q(2))*XX+Q(3))*XX+Q(4))              &
                 *XX+Q(5)
            RESULT = (SUMP / SUMQ) * X
            IF (JINT .EQ. 2) RESULT = RESULT * EXP(-X)
         ELSE IF ((JINT .EQ. 1) .AND. (X .GT. XMAX)) THEN
                  RESULT = XINF
         ELSE
!--------------------------------------------------------------------
!  15.0 .LE. ABS(ARG)
!--------------------------------------------------------------------
            XX = ONE / X - REC15
            SUMP = ((((((PP(1)*XX+PP(2))*XX+PP(3))*XX+                  &
                 PP(4))*XX+PP(5))*XX+PP(6))*XX+PP(7))*XX+PP(8)
            SUMQ = (((((XX+QQ(1))*XX+QQ(2))*XX+QQ(3))*XX+               &
                 QQ(4))*XX+QQ(5))*XX+QQ(6)
            RESULT = SUMP / SUMQ
            IF (JINT .NE. 1) THEN
                  RESULT = (RESULT + PBAR) / SQRT(X)
               ELSE
!--------------------------------------------------------------------
!  Calculation reformulated to avoid premature overflow
!--------------------------------------------------------------------
                  IF (X .GT. XMAX-ONE5) THEN
                        A = EXP(X-FORTY)
                        B = EXP40
                     ELSE
                        A = EXP(X)
                        B = ONE
                  END IF
                  RESULT = ((RESULT * A + PBAR * A) /                   &
                        SQRT(X)) * B
!--------------------------------------------------------------------
!  Error return for ABS(ARG) .GT. XMAX
!--------------------------------------------------------------------
            END IF
      END IF
      IF (ARG .LT. ZERO) RESULT = -RESULT
      RETURN
!----------- Last line of CALCI1 -----------
      END





!***********************************************************************************************************************************
!  BESI1
!***********************************************************************************************************************************

      real(wp) FUNCTION BESI1(X)
! This long precision subprogram computes approximate values for
!   modified Bessel functions of the first kind of order one for
!   arguments ABS(ARG) .LE. XMAX  (see comments heading CALCI1).
      INTEGER JINT
      real(wp), intent(in) :: x
      real(wp) ::  RES
!
      JINT=1
      CALL CALCI1(X,RES,JINT)
      BESI1=RES

    END FUNCTION BESI1





!***********************************************************************************************************************************
!  BESEI1
!***********************************************************************************************************************************

!S    REAL
      real(wp) FUNCTION BESEI1(X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the first kind of order one
!   multiplied by EXP(-ABS(X)), where EXP is the
!   exponential function, ABS is the absolute value, and X
!   is any argument.
!
!--------------------------------------------------------------------
      INTEGER JINT
!S    REAL
      real(wp)                                                  &
          X, RESULT
!--------------------------------------------------------------------
      JINT=2
      CALL CALCI1(X,RESULT,JINT)
      BESEI1=RESULT
      RETURN
!---------- Last line of BESEI1 ----------
      END





!***********************************************************************************************************************************
!  RIBESL
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE RIBESL(X,ALPHA,NB,IZE,B,NCALC)
!-------------------------------------------------------------------
!
!  This routine calculates Bessel functions I SUB(N+ALPHA) (X)
!  for non-negative argument X, and non-negative order N+ALPHA,
!  with or without exponential scaling.
!
!
! Explanation of variables in the calling sequence
!
! X     - Working precision non-negative real argument for which
!         I's or exponentially scaled I's (I*EXP(-X))
!         are to be calculated.  If I's are to be calculated,
!         X must be less than EXPARG (see below).
! ALPHA - Working precision fractional part of order for which
!         I's or exponentially scaled I's (I*EXP(-X)) are
!         to be calculated.  0 .LE. ALPHA .LT. 1.0.
! NB    - Integer number of functions to be calculated, NB .GT. 0.
!         The first function calculated is of order ALPHA, and the
!         last is of order (NB - 1 + ALPHA).
! IZE   - Integer type.  IZE = 1 if unscaled I's are to calculated,
!         and 2 if exponentially scaled I's are to be calculated.
! B     - Working precision output vector of length NB.  If the routine
!         terminates normally (NCALC=NB), the vector B contains the
!         functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
!         corresponding exponentially scaled functions.
! NCALC - Integer output variable indicating possible errors.
!         Before using the vector B, the user should check that
!         NCALC=NB, i.e., all orders have been calculated to
!         the desired accuracy.  See error returns below.
!
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   it     = Number of bits in the mantissa of a working precision
!            variable
!   NSIG   = Decimal significance desired.  Should be set to
!            INT(LOG10(2)*it+1).  Setting NSIG lower will result
!            in decreased accuracy while setting NSIG higher will
!            increase CPU time without increasing accuracy.  The
!            truncation error is limited to a relative error of
!            T=.5*10**(-NSIG).
!   ENTEN  = 10.0 ** K, where K is the largest integer such that
!            ENTEN is machine-representable in working precision
!   ENSIG  = 10.0 ** NSIG
!   RTNSIG = 10.0 ** (-K) for the smallest integer K such that
!            K .GE. NSIG/4
!   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
!   XLARGE = Upper limit on the magnitude of X when IZE=2.  Bear
!            in mind that if ABS(X)=N, then at least N iterations
!            of the backward recursion will be executed.  The value
!            of 10.0 ** 4 is used on every machine.
!   EXPARG = Largest working precision argument that the library
!            EXP routine can handle and upper limit on the
!            magnitude of X when IZE=1; approximately
!            LOG(beta**maxexp)
!
!
!     Approximate values for some important machines are:
!
!                        beta       minexp      maxexp       it
!
!  CRAY-1        (S.P.)    2        -8193        8191        48
!  Cyber 180/855
!    under NOS   (S.P.)    2         -975        1070        48
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)    2         -126         128        24
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)    2        -1022        1024        53
!  IBM 3033      (D.P.)   16          -65          63        14
!  VAX           (S.P.)    2         -128         127        24
!  VAX D-Format  (D.P.)    2         -128         127        56
!  VAX G-Format  (D.P.)    2        -1024        1023        53
!
!
!                        NSIG       ENTEN       ENSIG      RTNSIG
!
! CRAY-1        (S.P.)    15       1.0E+2465   1.0E+15     1.0E-4
! Cyber 180/855
!   under NOS   (S.P.)    15       1.0E+322    1.0E+15     1.0E-4
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)    16       1.0D+308    1.0D+16     1.0D-4
! IBM 3033      (D.P.)     5       1.0D+75     1.0D+5      1.0D-2
! VAX           (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
! VAX D-Format  (D.P.)    17       1.0D+38     1.0D+17     1.0D-5
! VAX G-Format  (D.P.)    16       1.0D+307    1.0D+16     1.0D-4
!
!
!                         ENMTEN      XLARGE   EXPARG
!
! CRAY-1        (S.P.)   1.84E-2466   1.0E+4    5677
! Cyber 180/855
!   under NOS   (S.P.)   1.25E-293    1.0E+4     741
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)   4.70E-38     1.0E+4      88
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)   8.90D-308    1.0D+4     709
! IBM 3033      (D.P.)   2.16D-78     1.0D+4     174
! VAX           (S.P.)   1.17E-38     1.0E+4      88
! VAX D-Format  (D.P.)   1.17D-38     1.0D+4      88
! VAX G-Format  (D.P.)   2.22D-308    1.0D+4     709
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  In case of an error,  NCALC .NE. NB, and not all I's are
!  calculated to the desired accuracy.
!
!  NCALC .LT. 0:  An argument is out of range. For example,
!     NB .LE. 0, IZE is not 1 or 2, or IZE=1 and ABS(X) .GE. EXPARG.
!     In this case, the B-vector is not calculated, and NCALC is
!     set to MIN0(NB,0)-1 so that NCALC .NE. NB.
!
!  NB .GT. NCALC .GT. 0: Not all requested function values could
!     be calculated accurately.  This usually occurs because NB is
!     much larger than ABS(X).  In this case, B(N) is calculated
!     to the desired accuracy for N .LE. NCALC, but precision
!     is lost for NCALC .LT. N .LE. NB.  If B(N) does not vanish
!     for N .GT. NCALC (because it is too small to be represented),
!     and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
!     significant figures of B(N) can be trusted.
!
!
! Intrinsic functions required are:
!
!     DBLE, EXP, GAMMA, INT, MAX, MIN, REAL, SQRT
!
!
! Acknowledgement
!
!  This program is based on a program written by David J.
!  Sookne (2) that computes values of the Bessel functions J or
!  I of real argument and integer order.  Modifications include
!  the restriction of the computation to the I Bessel function
!  of non-negative real argument, the extension of the computation
!  to arbitrary positive order, the inclusion of optional
!  exponential scaling, and the elimination of most underflow.
!  An earlier version was published in (3).
!
! References: "A Note on Backward Recurrence Algorithms," Olver,
!              F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
!              pp 941-947.
!
!             "Bessel Functions of Real Argument and Integer Order,"
!              Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
!              125-132.
!
!             "ALGORITHM 597, Sequence of Modified Bessel Functions
!              of the First Kind," Cody, W. J., Trans. Math. Soft.,
!              1983, pp. 242-245.
!
!  Latest modification: May 30, 1989
!
!  Modified by: W. J. Cody and L. Stoltz
!               Applied Mathematics Division
!               Argonne National Laboratory
!               Argonne, IL  60439
!
!-------------------------------------------------------------------
      INTEGER IZE,K,L,MAGX,N,NB,NBMX,NCALC,NEND,NSIG,NSTART
!S    REAL              GAMMA,
      real(wp) ALPHA,B,CONST,CONV,EM,EMPAL,EMP2AL,EN,ENSIG,              &
       EXPARG,FUNC,HALF,HALFX,ONE,P,PLAST,POLD,PSAVE,PSAVEL,      &
       RTNSIG,SUM,TEMPA,TEMPB,TEMPC,TEST,TOVER,TWO,X,XLARGE,ZERO
      DIMENSION B(NB)
      real(wp), parameter :: ENMTEN = tiny(0._wp)
!-------------------------------------------------------------------
!  Mathematical constants
!-------------------------------------------------------------------
!S    DATA ONE,TWO,ZERO,HALF,CONST/1.0E0,2.0E0,0.0E0,0.5E0,1.585E0/
      DATA ONE,TWO,ZERO,HALF,CONST/1.0D0,2.0D0,0.0D0,0.5D0,1.585D0/
!-------------------------------------------------------------------
!  Machine-dependent parameters
!-------------------------------------------------------------------
!S    DATA NSIG,XLARGE,EXPARG /8,1.0E4,88.0E0/
!S    DATA ENTEN,ENSIG,RTNSIG/1.0E38,1.0E8,1.0E-2/
!S    DATA ENMTEN/4.7E-38/
      DATA NSIG,XLARGE,EXPARG /16,1.0D4,709.0D0/
      DATA ENSIG,RTNSIG/1.0D16,1.0D-4/
      
!-------------------------------------------------------------------
!  Statement functions for conversion
!-------------------------------------------------------------------
!S    CONV(N) = REAL(N)
!S    FUNC(X) = GAMMA(X)
      CONV(N) = DBLE(N)
      FUNC(X) = gamma(X)
!-------------------------------------------------------------------
! Check for X, NB, OR IZE out of range.
!-------------------------------------------------------------------
      IF ((NB.GT.0) .AND. (X .GE. ZERO) .AND.                           &
          (ALPHA .GE. ZERO) .AND. (ALPHA .LT. ONE) .AND.                &
          (((IZE .EQ. 1) .AND. (X .LE. EXPARG)) .OR.                    &
           ((IZE .EQ. 2) .AND. (X .LE. XLARGE)))) THEN
!-------------------------------------------------------------------
! Use 2-term ascending series for small X
!-------------------------------------------------------------------
            NCALC = NB
            MAGX = INT(X)
            IF (X .GE. RTNSIG) THEN
!-------------------------------------------------------------------
! Initialize the forward sweep, the P-sequence of Olver
!-------------------------------------------------------------------
                  NBMX = NB-MAGX
                  N = MAGX+1
                  EN = CONV(N+N) + (ALPHA+ALPHA)
                  PLAST = ONE
                  P = EN / X
!-------------------------------------------------------------------
! Calculate general significance test
!-------------------------------------------------------------------
                  TEST = ENSIG + ENSIG
                  IF (2*MAGX .GT. 5*NSIG) THEN
                        TEST = SQRT(TEST*P)
                     ELSE
                        TEST = TEST / CONST**MAGX
                  END IF
                  IF (NBMX .GE. 3) THEN
!-------------------------------------------------------------------
! Calculate P-sequence until N = NB-1.  Check for possible overflow.
!-------------------------------------------------------------------
                     TOVER = ENTEN / ENSIG
                     NSTART = MAGX+2
                     NEND = NB - 1
                     DO 100 K = NSTART, NEND
                        N = K
                        EN = EN + TWO
                        POLD = PLAST
                        PLAST = P
                        P = EN * PLAST/X + POLD
                        IF (P .GT. TOVER) THEN
!-------------------------------------------------------------------
! To avoid overflow, divide P-sequence by TOVER.  Calculate
! P-sequence until ABS(P) .GT. 1.
!-------------------------------------------------------------------
                           TOVER = ENTEN
                           P = P / TOVER
                           PLAST = PLAST / TOVER
                           PSAVE = P
                           PSAVEL = PLAST
                           NSTART = N + 1
   60                      N = N + 1
                              EN = EN + TWO
                              POLD = PLAST
                              PLAST = P
                              P = EN * PLAST/X + POLD
                           IF (P .LE. ONE) GO TO 60
                           TEMPB = EN / X
!-------------------------------------------------------------------
! Calculate backward test, and find NCALC, the highest N
! such that the test is passed.
!-------------------------------------------------------------------
                           TEST = POLD*PLAST / ENSIG
                           TEST = TEST*(HALF-HALF/(TEMPB*TEMPB))
                           P = PLAST * TOVER
                           N = N - 1
                           EN = EN - TWO
                           NEND = MIN0(NB,N)
                           DO 80 L = NSTART, NEND
                              NCALC = L
                              POLD = PSAVEL
                              PSAVEL = PSAVE
                              PSAVE = EN * PSAVEL/X + POLD
                              IF (PSAVE*PSAVEL .GT. TEST) GO TO 90
   80                      CONTINUE
                           NCALC = NEND + 1
   90                      NCALC = NCALC - 1
                           GO TO 120
                        END IF
  100                CONTINUE
                     N = NEND
                     EN = CONV(N+N) + (ALPHA+ALPHA)
!-------------------------------------------------------------------
! Calculate special significance test for NBMX .GT. 2.
!-------------------------------------------------------------------
                     TEST = MAX(TEST,SQRT(PLAST*ENSIG)*SQRT(P+P))
                  END IF
!-------------------------------------------------------------------
! Calculate P-sequence until significance test passed.
!-------------------------------------------------------------------
  110             N = N + 1
                     EN = EN + TWO
                     POLD = PLAST
                     PLAST = P
                     P = EN * PLAST/X + POLD
                  IF (P .LT. TEST) GO TO 110
!-------------------------------------------------------------------
! Initialize the backward recursion and the normalization sum.
!-------------------------------------------------------------------
  120             N = N + 1
                  EN = EN + TWO
                  TEMPB = ZERO
                  TEMPA = ONE / P
                  EM = CONV(N) - ONE
                  EMPAL = EM + ALPHA
                  EMP2AL = (EM - ONE) + (ALPHA + ALPHA)
                  SUM = TEMPA * EMPAL * EMP2AL / EM
                  NEND = N - NB
                  IF (NEND .LT. 0) THEN
!-------------------------------------------------------------------
! N .LT. NB, so store B(N) and set higher orders to zero.
!-------------------------------------------------------------------
                        B(N) = TEMPA
                        NEND = -NEND
                        DO 130 L = 1, NEND
  130                      B(N+L) = ZERO
                     ELSE
                        IF (NEND .GT. 0) THEN
!-------------------------------------------------------------------
! Recur backward via difference equation, calculating (but
! not storing) B(N), until N = NB.
!-------------------------------------------------------------------
                           DO 140 L = 1, NEND
                              N = N - 1
                              EN = EN - TWO
                              TEMPC = TEMPB
                              TEMPB = TEMPA
                              TEMPA = (EN*TEMPB) / X + TEMPC
                              EM = EM - ONE
                              EMP2AL = EMP2AL - ONE
                              IF (N .EQ. 1) GO TO 150
                              IF (N .EQ. 2) EMP2AL = ONE
                              EMPAL = EMPAL - ONE
                              SUM = (SUM + TEMPA*EMPAL) * EMP2AL / EM
  140                      CONTINUE
                        END IF
!-------------------------------------------------------------------
! Store B(NB)
!-------------------------------------------------------------------
  150                   B(N) = TEMPA
                        IF (NB .LE. 1) THEN
                           SUM = (SUM + SUM) + TEMPA
                           GO TO 230
                        END IF
!-------------------------------------------------------------------
! Calculate and Store B(NB-1)
!-------------------------------------------------------------------
                        N = N - 1
                        EN = EN - TWO
                        B(N)  = (EN*TEMPA) / X + TEMPB
                        IF (N .EQ. 1) GO TO 220
                        EM = EM - ONE
                        EMP2AL = EMP2AL - ONE
                        IF (N .EQ. 2) EMP2AL = ONE
                        EMPAL = EMPAL - ONE
                        SUM = (SUM + B(N)*EMPAL) * EMP2AL / EM
                  END IF
                  NEND = N - 2
                  IF (NEND .GT. 0) THEN
!-------------------------------------------------------------------
! Calculate via difference equation and store B(N), until N = 2.
!-------------------------------------------------------------------
                     DO 200 L = 1, NEND
                        N = N - 1
                        EN = EN - TWO
                        B(N) = (EN*B(N+1)) / X +B(N+2)
                        EM = EM - ONE
                        EMP2AL = EMP2AL - ONE
                        IF (N .EQ. 2) EMP2AL = ONE
                        EMPAL = EMPAL - ONE
                        SUM = (SUM + B(N)*EMPAL) * EMP2AL / EM
  200                CONTINUE
                  END IF
!-------------------------------------------------------------------
! Calculate B(1)
!-------------------------------------------------------------------
                  B(1) = TWO*EMPAL*B(2) / X + B(3)
  220             SUM = (SUM + SUM) + B(1)
!-------------------------------------------------------------------
! Normalize.  Divide all B(N) by sum.
!-------------------------------------------------------------------
  230             IF (ALPHA .NE. ZERO)                                  &
                     SUM = SUM * FUNC(ONE+ALPHA) * (X*HALF)**(-ALPHA)
                  IF (IZE .EQ. 1) SUM = SUM * EXP(-X)
                  TEMPA = ENMTEN
                  IF (SUM .GT. ONE) TEMPA = TEMPA * SUM
                  DO 260 N = 1, NB
                     IF (B(N) .LT. TEMPA) B(N) = ZERO
                     B(N) = B(N) / SUM
  260             CONTINUE
                  RETURN
!-------------------------------------------------------------------
! Two-term ascending series for small X.
!-------------------------------------------------------------------
               ELSE
                  TEMPA = ONE
                  EMPAL = ONE + ALPHA
                  HALFX = ZERO
                  IF (X .GT. ENMTEN) HALFX = HALF * X
                  IF (ALPHA .NE. ZERO) TEMPA = HALFX**ALPHA /FUNC(EMPAL)
                  IF (IZE .EQ. 2) TEMPA = TEMPA * EXP(-X)
                  TEMPB = ZERO
                  IF ((X+ONE) .GT. ONE) TEMPB = HALFX * HALFX
                  B(1) = TEMPA + TEMPA*TEMPB / EMPAL
                  IF ((X .NE. ZERO) .AND. (B(1) .EQ. ZERO)) NCALC = 0
                  IF (NB .GT. 1) THEN
                     IF (X .EQ. ZERO) THEN
                           DO 310 N = 2, NB
                              B(N) = ZERO
  310                      CONTINUE
                        ELSE
!-------------------------------------------------------------------
! Calculate higher-order functions.
!-------------------------------------------------------------------
                           TEMPC = HALFX
                           TOVER = (ENMTEN + ENMTEN) / X
                           IF (TEMPB .NE. ZERO) TOVER = ENMTEN / TEMPB
                           DO 340 N = 2, NB
                              TEMPA = TEMPA / EMPAL
                              EMPAL = EMPAL + ONE
                              TEMPA = TEMPA * TEMPC
                              IF (TEMPA .LE. TOVER*EMPAL) TEMPA = ZERO
                              B(N) = TEMPA + TEMPA*TEMPB / EMPAL
                              IF ((B(N) .EQ. ZERO) .AND. (NCALC .GT. N))&
                                   NCALC = N-1
  340                      CONTINUE
                     END IF
                  END IF
            END IF
         ELSE
            NCALC = MIN0(NB,0)-1
      END IF
      RETURN
!---------- Last line of RIBESL ----------
      END





!***********************************************************************************************************************************
!  CALCK0
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE CALCK0(ARG,RESULT,JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functions of the second kind
!   and order zero, K0(X) and EXP(X)*K0(X), for real
!   arguments X.  It contains two function type subprograms, BESK0
!   and BESEK0, and one subroutine type subprogram, CALCK0.
!   the calling statements for the primary entries are
!
!                   Y=BESK0(X)
!   and
!                   Y=BESEK0(X)
!
!   where the entry points correspond to the functions K0(X) and
!   EXP(X)*K0(X), respectively.  The routine CALCK0 is
!   intended for internal packet use only, all computations within
!   the packet being concentrated in this routine.  The function
!   subprograms invoke CALCK0 with the statement
!          CALL CALCK0(ARG,RESULT,JINT)
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALCK0
!       Call              ARG                  RESULT          JINT
!
!     BESK0(ARG)   0 .LT. ARG .LE. XMAX       K0(ARG)           1
!     BESEK0(ARG)     0 .LT. ARG           EXP(ARG)*K0(ARG)     2
!
!   The main computation evaluates slightly modified forms of near
!   minimax rational approximations generated by Russon and Blair,
!   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461,
!   1969.  This transportable program is patterned after the
!   machine-dependent FUNPACK packet NATSK0, but cannot match that
!   version for efficiency or accuracy.  This version uses rational
!   functions that theoretically approximate K-SUB-0(X) to at
!   least 18 significant decimal digits.  The accuracy achieved
!   depends on the arithmetic system, the compiler, the intrinsic
!   functions, and proper selection of the machine-dependent
!   constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   XSMALL = Argument below which BESK0 and BESEK0 may
!            each be represented by a constant and a log.
!            largest X such that  1.0 + X = 1.0  to machine
!            precision.
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   XMAX   = Largest argument acceptable to BESK0;  Solution to
!            equation:
!               W(X) * (1-1/8X+9/128X**2) = beta**minexp
!            where  W(X) = EXP(-X)*SQRT(PI/2X)
!
!
!     Approximate values for some important machines are:
!
!
!                           beta       minexp       maxexp
!
!  CRAY-1        (S.P.)       2        -8193         8191
!  Cyber 180/185
!    under NOS   (S.P.)       2         -975         1070
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)       2         -126          128
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)       2        -1022         1024
!  IBM 3033      (D.P.)      16          -65           63
!  VAX D-Format  (D.P.)       2         -128          127
!  VAX G-Format  (D.P.)       2        -1024         1023
!
!
!                          XSMALL       XINF         XMAX
!
! CRAY-1        (S.P.)    3.55E-15   5.45E+2465    5674.858
! Cyber 180/855
!   under NOS   (S.P.)    1.77E-15   1.26E+322      672.788
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)    5.95E-8    3.40E+38        85.337
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)    1.11D-16   1.79D+308      705.342
! IBM 3033      (D.P.)    1.11D-16   7.23D+75       177.852
! VAX D-Format  (D.P.)    6.95D-18   1.70D+38        86.715
! VAX G-Format  (D.P.)    5.55D-17   8.98D+307      706.728
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns the value XINF for ARG .LE. 0.0, and the
!  BESK0 entry returns the value 0.0 for ARG .GT. XMAX.
!
!
!  Intrinsic functions required are:
!
!     EXP, LOG, SQRT
!
!  Latest modification: March 19, 1990
!
!  Authors: W. J. Cody and Laura Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL 60439
!
!--------------------------------------------------------------------
      INTEGER I,JINT
!S    REAL
      real(wp)                                                  &
          ARG,F,G,ONE,P,PP,Q,QQ,RESULT,SUMF,SUMG,SUMP,SUMQ,TEMP,        &
          X,XINF,XMAX,XSMALL,XX,ZERO
      DIMENSION P(6),Q(2),PP(10),QQ(10),F(4),G(3)
!--------------------------------------------------------------------
!  Mathematical constants
!--------------------------------------------------------------------
!S    DATA ONE/1.0E0/,ZERO/0.0E0/
      DATA ONE/1.0D0/,ZERO/0.0D0/
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
!S    DATA XSMALL/5.95E-8/,XINF/3.40E+38/,XMAX/ 85.337E0/
      DATA XSMALL/1.11D-16/,XMAX/705.342_wp/
!--------------------------------------------------------------------
!
!     Coefficients for XSMALL .LE.  ARG  .LE. 1.0
!
!--------------------------------------------------------------------
!S    DATA   P/ 5.8599221412826100000E-04, 1.3166052564989571850E-01,
!S   1          1.1999463724910714109E+01, 4.6850901201934832188E+02,
!S   2          5.9169059852270512312E+03, 2.4708152720399552679E+03/
!S    DATA   Q/-2.4994418972832303646E+02, 2.1312714303849120380E+04/
!S    DATA   F/-1.6414452837299064100E+00,-2.9601657892958843866E+02,
!S   1         -1.7733784684952985886E+04,-4.0320340761145482298E+05/
!S    DATA   G/-2.5064972445877992730E+02, 2.9865713163054025489E+04,
!S   1         -1.6128136304458193998E+06/
      DATA   P/ 5.8599221412826100000D-04, 1.3166052564989571850D-01,   &
                1.1999463724910714109D+01, 4.6850901201934832188D+02,   &
                5.9169059852270512312D+03, 2.4708152720399552679D+03/
      DATA   Q/-2.4994418972832303646D+02, 2.1312714303849120380D+04/
      DATA   F/-1.6414452837299064100D+00,-2.9601657892958843866D+02,   &
               -1.7733784684952985886D+04,-4.0320340761145482298D+05/
      DATA   G/-2.5064972445877992730D+02, 2.9865713163054025489D+04,   &
               -1.6128136304458193998D+06/
!--------------------------------------------------------------------
!
!     Coefficients for  1.0 .LT. ARG
!
!--------------------------------------------------------------------
!S    DATA  PP/ 1.1394980557384778174E+02, 3.6832589957340267940E+03,
!S   1          3.1075408980684392399E+04, 1.0577068948034021957E+05,
!S   2          1.7398867902565686251E+05, 1.5097646353289914539E+05,
!S   3          7.1557062783764037541E+04, 1.8321525870183537725E+04,
!S   4          2.3444738764199315021E+03, 1.1600249425076035558E+02/
!S    DATA  QQ/ 2.0013443064949242491E+02, 4.4329628889746408858E+03,
!S   1          3.1474655750295278825E+04, 9.7418829762268075784E+04,
!S   2          1.5144644673520157801E+05, 1.2689839587977598727E+05,
!S   3          5.8824616785857027752E+04, 1.4847228371802360957E+04,
!S   4          1.8821890840982713696E+03, 9.2556599177304839811E+01/
      DATA  PP/ 1.1394980557384778174D+02, 3.6832589957340267940D+03,   &
                3.1075408980684392399D+04, 1.0577068948034021957D+05,   &
                1.7398867902565686251D+05, 1.5097646353289914539D+05,   &
                7.1557062783764037541D+04, 1.8321525870183537725D+04,   &
                2.3444738764199315021D+03, 1.1600249425076035558D+02/
      DATA  QQ/ 2.0013443064949242491D+02, 4.4329628889746408858D+03,   &
                3.1474655750295278825D+04, 9.7418829762268075784D+04,   &
                1.5144644673520157801D+05, 1.2689839587977598727D+05,   &
                5.8824616785857027752D+04, 1.4847228371802360957D+04,   &
                1.8821890840982713696D+03, 9.2556599177304839811D+01/
!--------------------------------------------------------------------
      X = ARG
      IF (X .GT. ZERO) THEN
            IF (X .LE. ONE) THEN
!--------------------------------------------------------------------
!     0.0 .LT.  ARG  .LE. 1.0
!--------------------------------------------------------------------
                  TEMP = LOG(X)
                  IF (X .LT. XSMALL) THEN
!--------------------------------------------------------------------
!     Return for small ARG
!--------------------------------------------------------------------
                        RESULT = P(6)/Q(2) - TEMP
                     ELSE
                        XX = X * X
                        SUMP = ((((P(1)*XX + P(2))*XX + P(3))*XX +      &
                               P(4))*XX + P(5))*XX + P(6)
                        SUMQ = (XX + Q(1))*XX + Q(2)
                        SUMF = ((F(1)*XX + F(2))*XX + F(3))*XX + F(4)
                        SUMG = ((XX + G(1))*XX + G(2))*XX + G(3)
                        RESULT = SUMP/SUMQ - XX*SUMF*TEMP/SUMG - TEMP
                        IF (JINT .EQ. 2) RESULT = RESULT * EXP(X)
                  END IF
               ELSE IF ((JINT .EQ. 1) .AND. (X .GT. XMAX)) THEN
!--------------------------------------------------------------------
!     Error return for ARG .GT. XMAX
!--------------------------------------------------------------------
                  RESULT = ZERO
               ELSE
!--------------------------------------------------------------------
!     1.0 .LT. ARG
!--------------------------------------------------------------------
                  XX = ONE / X
                  SUMP = PP(1)
                  DO 120 I = 2, 10
                     SUMP = SUMP*XX + PP(I)
  120             CONTINUE
                  SUMQ = XX
                  DO 140 I = 1, 9
                     SUMQ = (SUMQ + QQ(I))*XX
  140             CONTINUE
                  SUMQ = SUMQ + QQ(10)
                  RESULT = SUMP / SUMQ / SQRT(X)
                  IF (JINT .EQ. 1) RESULT = RESULT * EXP(-X)
            END IF
         ELSE
!--------------------------------------------------------------------
!     Error return for ARG .LE. 0.0
!--------------------------------------------------------------------
            RESULT = XINF
      END IF
!--------------------------------------------------------------------
!     Update error counts, etc.
!--------------------------------------------------------------------
      RETURN
!---------- Last line of CALCK0 ----------
      END





!***********************************************************************************************************************************
!  BESK0
!***********************************************************************************************************************************

!S    REAL
real(wp) FUNCTION BESK0(X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order zero
!   for arguments 0.0 .LT. ARG .LE. XMAX (see comments heading
!   CALCK0).
!
!  Authors: W. J. Cody and Laura Stoltz
!
!  Latest Modification: January 19, 1988
!
!--------------------------------------------------------------------
INTEGER JINT
!S    REAL
real(wp)  X, RESULT
!--------------------------------------------------------------------
JINT = 1
CALL CALCK0(X,RESULT,JINT)
BESK0 = RESULT
RETURN
!---------- Last line of BESK0 ----------
END





!***********************************************************************************************************************************
!  BESEK0
!***********************************************************************************************************************************

!S    REAL
real(wp)  FUNCTION BESEK0(X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order zero
!   multiplied by the Exponential function, for arguments
!   0.0 .LT. ARG.
!
!  Authors: W. J. Cody and Laura Stoltz
!
!  Latest Modification: January 19, 1988
!
!--------------------------------------------------------------------
INTEGER JINT
!S    REAL
real(wp)                                                  &
    X, RESULT
!--------------------------------------------------------------------
JINT = 2
CALL CALCK0(X,RESULT,JINT)
BESEK0 = RESULT

END





!***********************************************************************************************************************************
!  CALCK1
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE CALCK1(ARG,RESULT,JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functions of the second kind
!   and order one,  K1(X)  and  EXP(X)*K1(X), for real arguments X.
!   It contains two function type subprograms, BESK1  and  BESEK1,
!   and one subroutine type subprogram, CALCK1.  The calling
!   statements for the primary entries are
!
!                   Y=BESK1(X)
!   and
!                   Y=BESEK1(X)
!
!   where the entry points correspond to the functions K1(X) and
!   EXP(X)*K1(X), respectively.  The routine CALCK1 is intended
!   for internal packet use only, all computations within the
!   packet being concentrated in this routine.  The function
!   subprograms invoke CALCK1 with the statement
!          CALL CALCK1(ARG,RESULT,JINT)
!   where the parameter usage is as follows
!
!      Function                      Parameters for CALCK1
!        Call             ARG                  RESULT          JINT
!
!     BESK1(ARG)  XLEAST .LT. ARG .LT. XMAX    K1(ARG)          1
!     BESEK1(ARG)     XLEAST .LT. ARG       EXP(ARG)*K1(ARG)    2
!
!   The main computation evaluates slightly modified forms of near
!   minimax rational approximations generated by Russon and Blair,
!   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461,
!   1969.  This transportable program is patterned after the
!   machine-dependent FUNPACK packet NATSK1, but cannot match that
!   version for efficiency or accuracy.  This version uses rational
!   functions that theoretically approximate K-SUB-1(X) to at
!   least 18 significant decimal digits.  The accuracy achieved
!   depends on the arithmetic system, the compiler, the intrinsic
!   functions, and proper selection of the machine-dependent
!   constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   XLEAST = Smallest acceptable argument, i.e., smallest machine
!            number X such that 1/X is machine representable.
!   XSMALL = Argument below which BESK1(X) and BESEK1(X) may
!            each be represented by 1/X.  A safe value is the
!            largest X such that  1.0 + X = 1.0  to machine
!            precision.
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   XMAX   = Largest argument acceptable to BESK1;  Solution to
!            equation:
!               W(X) * (1+3/8X-15/128X**2) = beta**minexp
!            where  W(X) = EXP(-X)*SQRT(PI/2X)
!
!
!     Approximate values for some important machines are:
!
!                           beta       minexp       maxexp
!
!  CRAY-1        (S.P.)       2        -8193         8191
!  Cyber 180/185
!    under NOS   (S.P.)       2         -975         1070
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)       2         -126          128
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)       2        -1022         1024
!  IBM 3033      (D.P.)      16          -65           63
!  VAX D-Format  (D.P.)       2         -128          127
!  VAX G-Format  (D.P.)       2        -1024         1023
!
!
!                         XLEAST     XSMALL      XINF       XMAX
!
! CRAY-1                1.84E-2466  3.55E-15  5.45E+2465  5674.858
! Cyber 180/855
!   under NOS   (S.P.)  3.14E-294   1.77E-15  1.26E+322    672.789
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)  1.18E-38    5.95E-8   3.40E+38      85.343
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)  2.23D-308   1.11D-16  1.79D+308    705.343
! IBM 3033      (D.P.)  1.39D-76    1.11D-16  7.23D+75     177.855
! VAX D-Format  (D.P.)  5.88D-39    6.95D-18  1.70D+38      86.721
! VAX G-Format  (D.P.)  1.12D-308   5.55D-17  8.98D+307    706.728
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns the value XINF for ARG .LE. 0.0 and the
!   BESK1 entry returns the value 0.0 for ARG .GT. XMAX.
!
!
!  Intrinsic functions required are:
!
!     LOG, SQRT, EXP
!
!
!  Authors: W. J. Cody and Laura Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL 60439
!
!  Latest modification: January 28, 1988
!
!--------------------------------------------------------------------
      INTEGER I,JINT
!S    REAL
      real(wp)                                                  &
          ARG,F,G,ONE,P,PP,Q,QQ,RESULT,SUMF,SUMG,                       &
          SUMP,SUMQ,X,XINF,XMAX,XSMALL,XX,ZERO
      DIMENSION P(5),Q(3),PP(11),QQ(9),F(5),G(3)
      
      real(wp), parameter :: xleast = tiny(0._wp)
!--------------------------------------------------------------------
!  Mathematical constants
!--------------------------------------------------------------------
!S    DATA ONE/1.0E0/,ZERO/0.0E0/
      DATA ONE/1.0D0/,ZERO/0.0D0/
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
!S    DATA XLEAST/1.18E-38/,XSMALL/5.95E-8/,XINF/3.40E+38/,
!S   1     XMAX/85.343E+0/
      DATA XSMALL/1.11D-16/,          &
           XMAX/705.343_wp/
!--------------------------------------------------------------------
!  Coefficients for  XLEAST .LE.  ARG  .LE. 1.0
!--------------------------------------------------------------------
!S    DATA   P/ 4.8127070456878442310E-1, 9.9991373567429309922E+1,
!S   1          7.1885382604084798576E+3, 1.7733324035147015630E+5,
!S   2          7.1938920065420586101E+5/
!S    DATA   Q/-2.8143915754538725829E+2, 3.7264298672067697862E+4,
!S   1         -2.2149374878243304548E+6/
!S    DATA   F/-2.2795590826955002390E-1,-5.3103913335180275253E+1,
!S   1         -4.5051623763436087023E+3,-1.4758069205414222471E+5,
!S   2         -1.3531161492785421328E+6/
!S    DATA   G/-3.0507151578787595807E+2, 4.3117653211351080007E+4,
!S   2         -2.7062322985570842656E+6/
      DATA   P/ 4.8127070456878442310D-1, 9.9991373567429309922D+1,     &
                7.1885382604084798576D+3, 1.7733324035147015630D+5,     &
                7.1938920065420586101D+5/
      DATA   Q/-2.8143915754538725829D+2, 3.7264298672067697862D+4,     &
               -2.2149374878243304548D+6/
      DATA   F/-2.2795590826955002390D-1,-5.3103913335180275253D+1,     &
               -4.5051623763436087023D+3,-1.4758069205414222471D+5,     &
               -1.3531161492785421328D+6/
      DATA   G/-3.0507151578787595807D+2, 4.3117653211351080007D+4,     &
               -2.7062322985570842656D+6/
!--------------------------------------------------------------------
!  Coefficients for  1.0 .LT.  ARG
!--------------------------------------------------------------------
!S    DATA  PP/ 6.4257745859173138767E-2, 7.5584584631176030810E+0,
!S   1          1.3182609918569941308E+2, 8.1094256146537402173E+2,
!S   2          2.3123742209168871550E+3, 3.4540675585544584407E+3,
!S   3          2.8590657697910288226E+3, 1.3319486433183221990E+3,
!S   4          3.4122953486801312910E+2, 4.4137176114230414036E+1,
!S   5          2.2196792496874548962E+0/
!S    DATA  QQ/ 3.6001069306861518855E+1, 3.3031020088765390854E+2,
!S   1          1.2082692316002348638E+3, 2.1181000487171943810E+3,
!S   2          1.9448440788918006154E+3, 9.6929165726802648634E+2,
!S   3          2.5951223655579051357E+2, 3.4552228452758912848E+1,
!S   4          1.7710478032601086579E+0/
      DATA  PP/ 6.4257745859173138767D-2, 7.5584584631176030810D+0,     &
                1.3182609918569941308D+2, 8.1094256146537402173D+2,     &
                2.3123742209168871550D+3, 3.4540675585544584407D+3,     &
                2.8590657697910288226D+3, 1.3319486433183221990D+3,     &
                3.4122953486801312910D+2, 4.4137176114230414036D+1,     &
                2.2196792496874548962D+0/
      DATA  QQ/ 3.6001069306861518855D+1, 3.3031020088765390854D+2,     &
                1.2082692316002348638D+3, 2.1181000487171943810D+3,     &
                1.9448440788918006154D+3, 9.6929165726802648634D+2,     &
                2.5951223655579051357D+2, 3.4552228452758912848D+1,     &
                1.7710478032601086579D+0/
!--------------------------------------------------------------------
      X = ARG
      IF (X .LT. XLEAST) THEN
!--------------------------------------------------------------------
!  Error return for  ARG  .LT. XLEAST
!--------------------------------------------------------------------
            RESULT = XINF
         ELSE IF (X .LE. ONE) THEN
!--------------------------------------------------------------------
!  XLEAST .LE.  ARG  .LE. 1.0
!--------------------------------------------------------------------
            IF (X .LT. XSMALL) THEN
!--------------------------------------------------------------------
!  Return for small ARG
!--------------------------------------------------------------------
                  RESULT = ONE / X
               ELSE
                  XX = X * X
                  SUMP = ((((P(1)*XX + P(2))*XX + P(3))*XX + P(4))*XX   &
                         + P(5))*XX + Q(3)
                  SUMQ = ((XX + Q(1))*XX + Q(2))*XX + Q(3)
                  SUMF = (((F(1)*XX + F(2))*XX + F(3))*XX + F(4))*XX    &
                         + F(5)
                  SUMG = ((XX + G(1))*XX + G(2))*XX + G(3)
                  RESULT = (XX * LOG(X) * SUMF/SUMG + SUMP/SUMQ) / X
                  IF (JINT .EQ. 2) RESULT = RESULT * EXP(X)
            END IF
         ELSE IF ((JINT .EQ. 1) .AND. (X .GT. XMAX)) THEN
!--------------------------------------------------------------------
!  Error return for  ARG  .GT. XMAX
!--------------------------------------------------------------------
            RESULT = ZERO
         ELSE
!--------------------------------------------------------------------
!  1.0 .LT.  ARG
!--------------------------------------------------------------------
            XX = ONE / X
            SUMP = PP(1)
            DO 120 I = 2, 11
               SUMP = SUMP * XX + PP(I)
  120       CONTINUE
            SUMQ = XX
            DO 140 I = 1, 8
               SUMQ = (SUMQ + QQ(I)) * XX
  140       CONTINUE
            SUMQ = SUMQ + QQ(9)
            RESULT = SUMP / SUMQ / SQRT(X)
            IF (JINT .EQ. 1) RESULT = RESULT * EXP(-X)
      END IF
      RETURN
!---------- Last line of CALCK1 ----------
      END





!***********************************************************************************************************************************
!  BESK1
!***********************************************************************************************************************************

real(wp) FUNCTION BESK1(X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order one
!   for arguments  XLEAST .LE. ARG .LE. XMAX.
!
!--------------------------------------------------------------------
INTEGER JINT
!S    REAL
real(wp)                                                  &
    X, RESULT
!--------------------------------------------------------------------
JINT = 1
CALL CALCK1(X,RESULT,JINT)
BESK1 = RESULT

END function besk1





!***********************************************************************************************************************************
!  BESEK1
!***********************************************************************************************************************************

real(wp) FUNCTION BESEK1(X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order one
!   multiplied by the exponential function, for arguments
!   XLEAST .LE. ARG .LE. XMAX.
!
!--------------------------------------------------------------------
INTEGER JINT

real(wp) X, RESULT
!--------------------------------------------------------------------
JINT = 2
CALL CALCK1(X,RESULT,JINT)
BESEK1 = RESULT

END function besek1





!***********************************************************************************************************************************
!  RKBESL
!
!  From http://www.netlib.org/specfun
!***********************************************************************************************************************************

      SUBROUTINE RKBESL(X,ALPHA,NB,IZE,BK,NCALC)
!-------------------------------------------------------------------
!
!  This FORTRAN 77 routine calculates modified Bessel functions
!  of the second kind, K SUB(N+ALPHA) (X), for non-negative
!  argument X, and non-negative order N+ALPHA, with or without
!  exponential scaling.
!
!  Explanation of variables in the calling sequence
!
!  Description of output values ..
!
! X     - Working precision non-negative real argument for which
!         K's or exponentially scaled K's (K*EXP(X))
!         are to be calculated.  If K's are to be calculated,
!         X must not be greater than XMAX (see below).
! ALPHA - Working precision fractional part of order for which
!         K's or exponentially scaled K's (K*EXP(X)) are
!         to be calculated.  0 .LE. ALPHA .LT. 1.0.
! NB    - Integer number of functions to be calculated, NB .GT. 0.
!         The first function calculated is of order ALPHA, and the
!         last is of order (NB - 1 + ALPHA).
! IZE   - Integer type.  IZE = 1 if unscaled K's are to be calculated,
!         and 2 if exponentially scaled K's are to be calculated.
! BK    - Working precision output vector of length NB.  If the
!         routine terminates normally (NCALC=NB), the vector BK
!         contains the functions K(ALPHA,X), ... , K(NB-1+ALPHA,X),
!         or the corresponding exponentially scaled functions.
!         If (0 .LT. NCALC .LT. NB), BK(I) contains correct function
!         values for I .LE. NCALC, and contains the ratios
!         K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
! NCALC - Integer output variable indicating possible errors.
!         Before using the vector BK, the user should check that
!         NCALC=NB, i.e., all orders have been calculated to
!         the desired accuracy.  See error returns below.
!
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   EPS    = The smallest positive floating-point number such that
!            1.0+EPS .GT. 1.0
!   XMAX   = Upper limit on the magnitude of X when IZE=1;  Solution
!            to equation:
!               W(X) * (1-1/8X+9/128X**2) = beta**minexp
!            where  W(X) = EXP(-X)*SQRT(PI/2X)
!   SQXMIN = Square root of beta**minexp
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   XMIN   = Smallest positive machine number; approximately
!            beta**minexp
!
!
!     Approximate values for some important machines are:
!
!                          beta       minexp      maxexp      EPS
!
!  CRAY-1        (S.P.)      2        -8193        8191    7.11E-15
!  Cyber 180/185
!    under NOS   (S.P.)      2         -975        1070    3.55E-15
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)      2         -126         128    1.19E-7
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)      2        -1022        1024    2.22D-16
!  IBM 3033      (D.P.)     16          -65          63    2.22D-16
!  VAX           (S.P.)      2         -128         127    5.96E-8
!  VAX D-Format  (D.P.)      2         -128         127    1.39D-17
!  VAX G-Format  (D.P.)      2        -1024        1023    1.11D-16
!
!
!                         SQXMIN       XINF        XMIN      XMAX
!
! CRAY-1        (S.P.)  6.77E-1234  5.45E+2465  4.59E-2467 5674.858
! Cyber 180/855
!   under NOS   (S.P.)  1.77E-147   1.26E+322   3.14E-294   672.788
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)  1.08E-19    3.40E+38    1.18E-38     85.337
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)  1.49D-154   1.79D+308   2.23D-308   705.342
! IBM 3033      (D.P.)  7.35D-40    7.23D+75    5.40D-79    177.852
! VAX           (S.P.)  5.42E-20    1.70E+38    2.94E-39     86.715
! VAX D-Format  (D.P.)  5.42D-20    1.70D+38    2.94D-39     86.715
! VAX G-Format  (D.P.)  7.46D-155   8.98D+307   5.57D-309   706.728
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  In case of an error, NCALC .NE. NB, and not all K's are
!  calculated to the desired accuracy.
!
!  NCALC .LT. -1:  An argument is out of range. For example,
!       NB .LE. 0, IZE is not 1 or 2, or IZE=1 and ABS(X) .GE.
!       XMAX.  In this case, the B-vector is not calculated,
!       and NCALC is set to MIN0(NB,0)-2  so that NCALC .NE. NB.
!  NCALC = -1:  Either  K(ALPHA,X) .GE. XINF  or
!       K(ALPHA+NB-1,X)/K(ALPHA+NB-2,X) .GE. XINF.  In this case,
!       the B-vector is not calculated.  Note that again
!       NCALC .NE. NB.
!
!  0 .LT. NCALC .LT. NB: Not all requested function values could
!       be calculated accurately.  BK(I) contains correct function
!       values for I .LE. NCALC, and contains the ratios
!       K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
!
!
! Intrinsic functions required are:
!
!     ABS, AINT, EXP, INT, LOG, MAX, MIN, SINH, SQRT
!
!
! Acknowledgement
!
!  This program is based on a program written by J. B. Campbell
!  (2) that computes values of the Bessel functions K of real
!  argument and real order.  Modifications include the addition
!  of non-scaled functions, parameterization of machine
!  dependencies, and the use of more accurate approximations
!  for SINH and SIN.
!
! References: "On Temme's Algorithm for the Modified Bessel
!              Functions of the Third Kind," Campbell, J. B.,
!              TOMS 6(4), Dec. 1980, pp. 581-586.
!
!             "A FORTRAN IV Subroutine for the Modified Bessel
!              Functions of the Third Kind of Real Order and Real
!              Argument," Campbell, J. B., Report NRC/ERB-925,
!              National Research Council, Canada.
!
!  Latest modification: May 30, 1989
!
!  Modified by: W. J. Cody and L. Stoltz
!               Applied Mathematics Division
!               Argonne National Laboratory
!               Argonne, IL  60439
!
!-------------------------------------------------------------------
      INTEGER I,IEND,ITEMP,IZE,J,K,M,MPLUS1,NB,NCALC
!S    REAL
      real(wp)                                                  &
          A,ALPHA,BLPHA,BK,BK1,BK2,C,D,DM,D1,D2,D3,ENU,EPS,ESTF,ESTM,   &
          EX,FOUR,F0,F1,F2,HALF,ONE,P,P0,Q,Q0,R,RATIO,S,SQXMIN,T,TINYX, &
          TWO,TWONU,TWOX,T1,T2,WMINF,X,XINF,XMAX,X2BY4,ZERO
      DIMENSION BK(1),P(8),Q(7),R(5),S(4),T(6),ESTM(6),ESTF(7)
!---------------------------------------------------------------------
!  Mathematical constants
!    A = LOG(2.D0) - Euler's constant
!    D = SQRT(2.D0/PI)
!---------------------------------------------------------------------
!S    DATA HALF,ONE,TWO,ZERO/0.5E0,1.0E0,2.0E0,0.0E0/
!S    DATA FOUR,TINYX/4.0E0,1.0E-10/
!S    DATA A/ 0.11593151565841244881E0/,D/0.797884560802865364E0/
      DATA HALF,ONE,TWO,ZERO/0.5D0,1.0D0,2.0D0,0.0D0/
      DATA FOUR,TINYX/4.0D0,1.0D-10/
      DATA A/ 0.11593151565841244881D0/,D/0.797884560802865364D0/
!---------------------------------------------------------------------
!  Machine dependent parameters
!---------------------------------------------------------------------
!S    DATA EPS/1.19E-7/,SQXMIN/1.08E-19/,XINF/3.40E+38/
!S    DATA XMIN/1.18E-38/,XMAX/85.337E0/
      DATA EPS/2.22D-16/,SQXMIN/1.49D-154/
      DATA XMAX/705.342_wp/
!---------------------------------------------------------------------
!  P, Q - Approximation for LOG(GAMMA(1+ALPHA))/ALPHA
!                                         + Euler's constant
!         Coefficients converted from hex to decimal and modified
!         by W. J. Cody, 2/26/82
!  R, S - Approximation for (1-ALPHA*PI/SIN(ALPHA*PI))/(2.D0*ALPHA)
!  T    - Approximation for SINH(Y)/Y
!---------------------------------------------------------------------
!S    DATA P/ 0.805629875690432845E00,    0.204045500205365151E02,
!S   1        0.157705605106676174E03,    0.536671116469207504E03,
!S   2        0.900382759291288778E03,    0.730923886650660393E03,
!S   3        0.229299301509425145E03,    0.822467033424113231E00/
!S    DATA Q/ 0.294601986247850434E02,    0.277577868510221208E03,
!S   1        0.120670325591027438E04,    0.276291444159791519E04,
!S   2        0.344374050506564618E04,    0.221063190113378647E04,
!S   3        0.572267338359892221E03/
!S    DATA R/-0.48672575865218401848E+0,  0.13079485869097804016E+2,
!S   1       -0.10196490580880537526E+3,  0.34765409106507813131E+3,
!S   2        0.34958981245219347820E-3/
!S    DATA S/-0.25579105509976461286E+2,  0.21257260432226544008E+3,
!S   1       -0.61069018684944109624E+3,  0.42269668805777760407E+3/
!S    DATA T/ 0.16125990452916363814E-9, 0.25051878502858255354E-7,
!S   1        0.27557319615147964774E-5, 0.19841269840928373686E-3,
!S   2        0.83333333333334751799E-2, 0.16666666666666666446E+0/
!S    DATA ESTM/5.20583E1, 5.7607E0, 2.7782E0, 1.44303E1, 1.853004E2,
!S   1          9.3715E0/
!S    DATA ESTF/4.18341E1, 7.1075E0, 6.4306E0, 4.25110E1, 1.35633E0,
!S   1          8.45096E1, 2.0E1/
      DATA P/ 0.805629875690432845D00,    0.204045500205365151D02,      &
              0.157705605106676174D03,    0.536671116469207504D03,      &
              0.900382759291288778D03,    0.730923886650660393D03,      &
              0.229299301509425145D03,    0.822467033424113231D00/
      DATA Q/ 0.294601986247850434D02,    0.277577868510221208D03,      &
              0.120670325591027438D04,    0.276291444159791519D04,      &
              0.344374050506564618D04,    0.221063190113378647D04,      &
              0.572267338359892221D03/
      DATA R/-0.48672575865218401848D+0,  0.13079485869097804016D+2,    &
             -0.10196490580880537526D+3,  0.34765409106507813131D+3,    &
              0.34958981245219347820D-3/
      DATA S/-0.25579105509976461286D+2,  0.21257260432226544008D+3,    &
             -0.61069018684944109624D+3,  0.42269668805777760407D+3/
      DATA T/ 0.16125990452916363814D-9, 0.25051878502858255354D-7,     &
              0.27557319615147964774D-5, 0.19841269840928373686D-3,     &
              0.83333333333334751799D-2, 0.16666666666666666446D+0/
      DATA ESTM/5.20583D1, 5.7607D0, 2.7782D0, 1.44303D1, 1.853004D2,   &
                9.3715D0/
      DATA ESTF/4.18341D1, 7.1075D0, 6.4306D0, 4.25110D1, 1.35633D0,    &
                8.45096D1, 2.0D1/
!---------------------------------------------------------------------
      EX = X
      ENU = ALPHA
      NCALC = MIN(NB,0)-2
      IF ((NB .GT. 0) .AND. ((ENU .GE. ZERO) .AND. (ENU .LT. ONE))      &
           .AND. ((IZE .GE. 1) .AND. (IZE .LE. 2)) .AND.                &
           ((IZE .NE. 1) .OR. (EX .LE. XMAX)) .AND.                     &
           (EX .GT. ZERO))  THEN
            K = 0
            IF (ENU .LT. SQXMIN) ENU = ZERO
            IF (ENU .GT. HALF) THEN
                  K = 1
                  ENU = ENU - ONE
            END IF
            TWONU = ENU+ENU
            IEND = NB+K-1
            C = ENU*ENU
            D3 = -C
            IF (EX .LE. ONE) THEN
!---------------------------------------------------------------------
!  Calculation of P0 = GAMMA(1+ALPHA) * (2/X)**ALPHA
!                 Q0 = GAMMA(1-ALPHA) * (X/2)**ALPHA
!---------------------------------------------------------------------
                  D1 = ZERO
                  D2 = P(1)
                  T1 = ONE
                  T2 = Q(1)
                  DO 10 I = 2,7,2
                     D1 = C*D1+P(I)
                     D2 = C*D2+P(I+1)
                     T1 = C*T1+Q(I)
                     T2 = C*T2+Q(I+1)
   10             CONTINUE
                  D1 = ENU*D1
                  T1 = ENU*T1
                  F1 = LOG(EX)
                  F0 = A+ENU*(P(8)-ENU*(D1+D2)/(T1+T2))-F1
                  Q0 = EXP(-ENU*(A-ENU*(P(8)+ENU*(D1-D2)/(T1-T2))-F1))
                  F1 = ENU*F0
                  P0 = EXP(F1)
!---------------------------------------------------------------------
!  Calculation of F0 =
!---------------------------------------------------------------------
                  D1 = R(5)
                  T1 = ONE
                  DO 20 I = 1,4
                     D1 = C*D1+R(I)
                     T1 = C*T1+S(I)
   20             CONTINUE
                  IF (ABS(F1) .LE. HALF) THEN
                        F1 = F1*F1
                        D2 = ZERO
                        DO 30 I = 1,6
                           D2 = F1*D2+T(I)
   30                   CONTINUE
                        D2 = F0+F0*F1*D2
                     ELSE
                        D2 = SINH(F1)/ENU
                  END IF
                  F0 = D2-ENU*D1/(T1*P0)
                  IF (EX .LE. TINYX) THEN
!--------------------------------------------------------------------
!  X.LE.1.0E-10
!  Calculation of K(ALPHA,X) and X*K(ALPHA+1,X)/K(ALPHA,X)
!--------------------------------------------------------------------
                        BK(1) = F0+EX*F0
                        IF (IZE .EQ. 1) BK(1) = BK(1)-EX*BK(1)
                        RATIO = P0/F0
                        C = EX*XINF
                        IF (K .NE. 0) THEN
!--------------------------------------------------------------------
!  Calculation of K(ALPHA,X) and X*K(ALPHA+1,X)/K(ALPHA,X),
!  ALPHA .GE. 1/2
!--------------------------------------------------------------------
                              NCALC = -1
                              IF (BK(1) .GE. C/RATIO) GO TO 500
                              BK(1) = RATIO*BK(1)/EX
                              TWONU = TWONU+TWO
                              RATIO = TWONU
                        END IF
                        NCALC = 1
                        IF (NB .EQ. 1) GO TO 500
!--------------------------------------------------------------------
!  Calculate  K(ALPHA+L,X)/K(ALPHA+L-1,X),  L  =  1, 2, ... , NB-1
!--------------------------------------------------------------------
                        NCALC = -1
                        DO 80 I = 2,NB
                           IF (RATIO .GE. C) GO TO 500
                           BK(I) = RATIO/EX
                           TWONU = TWONU+TWO
                           RATIO = TWONU
   80                   CONTINUE
                        NCALC = 1
                        GO TO 420
                     ELSE
!--------------------------------------------------------------------
!  1.0E-10 .LT. X .LE. 1.0
!--------------------------------------------------------------------
                        C = ONE
                        X2BY4 = EX*EX/FOUR
                        P0 = HALF*P0
                        Q0 = HALF*Q0
                        D1 = -ONE
                        D2 = ZERO
                        BK1 = ZERO
                        BK2 = ZERO
                        F1 = F0
                        F2 = P0
  100                   D1 = D1+TWO
                        D2 = D2+ONE
                        D3 = D1+D3
                        C = X2BY4*C/D2
                        F0 = (D2*F0+P0+Q0)/D3
                        P0 = P0/(D2-ENU)
                        Q0 = Q0/(D2+ENU)
                        T1 = C*F0
                        T2 = C*(P0-D2*F0)
                        BK1 = BK1+T1
                        BK2 = BK2+T2
                        IF ((ABS(T1/(F1+BK1)) .GT. EPS) .OR.            &
                           (ABS(T2/(F2+BK2)) .GT. EPS))  GO TO 100
                        BK1 = F1+BK1
                        BK2 = TWO*(F2+BK2)/EX
                        IF (IZE .EQ. 2) THEN
                              D1 = EXP(EX)
                              BK1 = BK1*D1
                              BK2 = BK2*D1
                        END IF
                        WMINF = ESTF(1)*EX+ESTF(2)
                  END IF
               ELSE IF (EPS*EX .GT. ONE) THEN
!--------------------------------------------------------------------
!  X .GT. ONE/EPS
!--------------------------------------------------------------------
                  NCALC = NB
                  BK1 = ONE / (D*SQRT(EX))
                  DO 110 I = 1, NB
                     BK(I) = BK1
  110             CONTINUE
                  GO TO 500
               ELSE
!--------------------------------------------------------------------
!  X .GT. 1.0
!--------------------------------------------------------------------
                  TWOX = EX+EX
                  BLPHA = ZERO
                  RATIO = ZERO
                  IF (EX .LE. FOUR) THEN
!--------------------------------------------------------------------
!  Calculation of K(ALPHA+1,X)/K(ALPHA,X),  1.0 .LE. X .LE. 4.0
!--------------------------------------------------------------------
                        D2 = AINT(ESTM(1)/EX+ESTM(2))
                        M = INT(D2)
                        D1 = D2+D2
                        D2 = D2-HALF
                        D2 = D2*D2
                        DO 120 I = 2,M
                           D1 = D1-TWO
                           D2 = D2-D1
                           RATIO = (D3+D2)/(TWOX+D1-RATIO)
  120                   CONTINUE
!--------------------------------------------------------------------
!  Calculation of I(|ALPHA|,X) and I(|ALPHA|+1,X) by backward
!    recurrence and K(ALPHA,X) from the wronskian
!--------------------------------------------------------------------
                        D2 = AINT(ESTM(3)*EX+ESTM(4))
                        M = INT(D2)
                        C = ABS(ENU)
                        D3 = C+C
                        D1 = D3-ONE
                        F1 = XMIN
                        F0 = (TWO*(C+D2)/EX+HALF*EX/(C+D2+ONE))*XMIN
                        DO 130 I = 3,M
                           D2 = D2-ONE
                           F2 = (D3+D2+D2)*F0
                           BLPHA = (ONE+D1/D2)*(F2+BLPHA)
                           F2 = F2/EX+F1
                           F1 = F0
                           F0 = F2
  130                   CONTINUE
                        F1 = (D3+TWO)*F0/EX+F1
                        D1 = ZERO
                        T1 = ONE
                        DO 140 I = 1,7
                           D1 = C*D1+P(I)
                           T1 = C*T1+Q(I)
  140                   CONTINUE
                        P0 = EXP(C*(A+C*(P(8)-C*D1/T1)-LOG(EX)))/EX
                        F2 = (C+HALF-RATIO)*F1/EX
                        BK1 = P0+(D3*F0-F2+F0+BLPHA)/(F2+F1+F0)*P0
                        IF (IZE .EQ. 1) BK1 = BK1*EXP(-EX)
                        WMINF = ESTF(3)*EX+ESTF(4)
                     ELSE
!--------------------------------------------------------------------
!  Calculation of K(ALPHA,X) and K(ALPHA+1,X)/K(ALPHA,X), by backward
!  recurrence, for  X .GT. 4.0
!--------------------------------------------------------------------
                        DM = AINT(ESTM(5)/EX+ESTM(6))
                        M = INT(DM)
                        D2 = DM-HALF
                        D2 = D2*D2
                        D1 = DM+DM
                        DO 160 I = 2,M
                           DM = DM-ONE
                           D1 = D1-TWO
                           D2 = D2-D1
                           RATIO = (D3+D2)/(TWOX+D1-RATIO)
                           BLPHA = (RATIO+RATIO*BLPHA)/DM
  160                   CONTINUE
                        BK1 = ONE/((D+D*BLPHA)*SQRT(EX))
                        IF (IZE .EQ. 1) BK1 = BK1*EXP(-EX)
                        WMINF = ESTF(5)*(EX-ABS(EX-ESTF(7)))+ESTF(6)
                  END IF
!--------------------------------------------------------------------
!  Calculation of K(ALPHA+1,X) from K(ALPHA,X) and
!    K(ALPHA+1,X)/K(ALPHA,X)
!--------------------------------------------------------------------
                  BK2 = BK1+BK1*(ENU+HALF-RATIO)/EX
            END IF
!--------------------------------------------------------------------
!  Calculation of 'NCALC', K(ALPHA+I,X), I  =  0, 1, ... , NCALC-1,
!  K(ALPHA+I,X)/K(ALPHA+I-1,X), I  =  NCALC, NCALC+1, ... , NB-1
!--------------------------------------------------------------------
            NCALC = NB
            BK(1) = BK1
            IF (IEND .EQ. 0) GO TO 500
            J = 2-K
            IF (J .GT. 0) BK(J) = BK2
            IF (IEND .EQ. 1) GO TO 500
            M = MIN(INT(WMINF-ENU),IEND)
            DO 190 I = 2,M
               T1 = BK1
               BK1 = BK2
               TWONU = TWONU+TWO
               IF (EX .LT. ONE) THEN
                     IF (BK1 .GE. (XINF/TWONU)*EX) GO TO 195
                     GO TO 187
                  ELSE
                     IF (BK1/EX .GE. XINF/TWONU) GO TO 195
               END IF
  187          CONTINUE
               BK2 = TWONU/EX*BK1+T1
               ITEMP = I
               J = J+1
               IF (J .GT. 0) BK(J) = BK2
  190       CONTINUE
  195       M = ITEMP
            IF (M .EQ. IEND) GO TO 500
            RATIO = BK2/BK1
            MPLUS1 = M+1
            NCALC = -1
            DO 410 I = MPLUS1,IEND
               TWONU = TWONU+TWO
               RATIO = TWONU/EX+ONE/RATIO
               J = J+1
               IF (J .GT. 1) THEN
                     BK(J) = RATIO
                  ELSE
                     IF (BK2 .GE. XINF/RATIO) GO TO 500
                     BK2 = RATIO*BK2
               END IF
  410       CONTINUE
            NCALC = MAX(MPLUS1-K,1)
            IF (NCALC .EQ. 1) BK(1) = BK2
            IF (NB .EQ. 1) GO TO 500
  420       J = NCALC+1
            DO 430 I = J,NB
               IF (BK(NCALC) .GE. XINF/BK(I)) GO TO 500
               BK(I) = BK(NCALC)*BK(I)
               NCALC = I
  430       CONTINUE
      END IF
  500 RETURN
!---------- Last line of RKBESL ----------
      END

end module funcs
