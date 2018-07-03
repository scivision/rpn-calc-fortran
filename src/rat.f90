module rat

use assert, only: wp

implicit none

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
   ! write(stderr, *)  'Error in RATNORM: denominator is zero.'
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
      N1 = int(T)
      T = D2
      D2 = D1
      D1 = int(T)
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

      END SUBROUTINE DEC_TO_FRAC



end module rat
