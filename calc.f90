!  Programmer:   David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  20771
!  Date:         December 28, 2005
!  Language:     Fortran-95
!  Version:      1.01d  (August 23, 2006)
!  Description:  Reverse Polish Notation (RPN) calculator.
!  Files:        Source file:
!                   rpn.f95                   Main program
!  Note:         This program assumes that the COMPLEX(wp) data type contains 8 bytes for the real component
!                and 8 bytes for the imaginary component, for a total of 16 bytes.  In other words, the real and
!                imaginary components are both DOUBLE PRECISION types.

!***********************************************************************************************************************************
!  Main program
!***********************************************************************************************************************************

      PROGRAM RPN
      use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit
      USE GLOBAL
      use funcs, only: rprintx, cprintx, printx, eval, rpush_stack, cpush_stack, push_stack, isrational, isreal, iscomplex

      IMPLICIT NONE

      real(wp), PARAMETER :: PI = 4._wp * atan(1._wp)
      real(wp), PARAMETER :: TWOPI = 2*pi
      INTEGER :: I, IDX, IERR, DEL, PTR, RN, RD
      DOUBLE PRECISION :: X
      COMPLEX(wp) :: CX
      CHARACTER(LEN=300) :: LINE, SUBSTR
      CHARACTER(LEN=100) :: FMTSTR, NUMSTR
      LOGICAL :: NUM_FLAG


!-----------------------------------------------------------------------------------------------------------------------------------

      print *, '  RPN Version '//VERSION

!
!     Initialize data.
!

      DEL = IACHAR('a') - IACHAR('A')                                               ! find ASCII position diff between 'A' and 'a'

      STACK = 0.0D0                                                                 ! clear the REAL stack
      REG = 0.0D0                                                                   ! clear the REAL registers
      LASTX = 0.0D0                                                                 ! clear the REAL LAST X register

      NN = 0.0D0                                                                    ! clear the REAL summation registers
      SUMX = 0.0D0
      SUMX2 = 0.0D0
      SUMY = 0.0D0
      SUMY2 = 0.0D0
      SUMXY = 0.0D0

      CSTACK = (0.0D0,0.0D0)                                                        ! clear the COMPLEX stack
      CREG = (0.0D0,0.0D0)                                                          ! clear the COMPLEX registers
      CLASTX = (0.0D0,0.0D0)                                                        ! clear the COMPLEX LAST X register

      CNN = (0.0D0,0.0D0)                                                           ! clear the COMPLEX summation registers
      CSUMX = (0.0D0,0.0D0)
      CSUMX2 = (0.0D0,0.0D0)
      CSUMY = (0.0D0,0.0D0)
      CSUMY2 = (0.0D0,0.0D0)
      CSUMXY = (0.0D0,0.0D0)

      RNSTACK = 0; RDSTACK = 1                                                      ! clear the RATIONAL stack
      RNREG = 0; RDREG = 1                                                          ! clear the RATIONAL registers
      RNLASTX = 0; RDLASTX = 1                                                      ! clear the RATIONAL LAST X register

      RNNN = 0; RDNN = 1                                                            ! clear the RATIONAL summation registers
      RNSUMX = 0; RDSUMX = 1
      RNSUMX2 = 0; RDSUMX2 = 1
      RNSUMY = 0; RDSUMY = 1
      RNSUMY2 = 0; RDSUMY2 = 1
      RNSUMXY = 0; RDSUMXY = 1

      ANGLE_MODE = INITIAL_ANGLE_MODE

      SELECT CASE (ANGLE_MODE)
         CASE (1)                                                                   ! deg
            ANGLE_FACTOR = PI/180.0D0
         CASE (2)                                                                   ! rad
            ANGLE_FACTOR = 1.0D0
         CASE (3)                                                                   ! grad
            ANGLE_FACTOR = PI/200.0D0
         CASE (4)                                                                   ! rev
            ANGLE_FACTOR = TWOPI
      END SELECT

      DISP_MODE = INITIAL_DISP_MODE                                                 ! set modes
      DISP_DIGITS = INITIAL_DISP_DIGITS
      DOMAIN_MODE = INITIAL_DOMAIN_MODE
      BASE_MODE = INITIAL_BASE_MODE
      FRACTION_MODE = INITIAL_FRACTION_MODE

      FRACTOL = INITIAL_FRACTOL                                                     ! set decimal-to-fraction tolerance

      CALL RANDOM_SEED                                                              ! init random number generator

!
!     Main loop.
!

      DO                                                                            ! loop once for each input line

         WRITE(stdout,'(A)', ADVANCE='NO') '  ? '
         READ (stdin,*, iostat=ierr) LINE
         if (ierr<0) stop

!
!     Convert the input line to all uppercase.
!

         LINE = ADJUSTL(LINE)                                                       ! remove leading blanks
         DO I = 1, LEN_TRIM(LINE)                                                   ! scan each character in line
            IF (LGE(LINE(I:I),'a') .AND. LLE(LINE(I:I),'z')) THEN                   ! if between 'a' and 'z'..
               LINE(I:I) = ACHAR(IACHAR(LINE(I:I)) - DEL)                           ! ..then convert to uppercase
            END IF
         END DO

!
!     Search for QUIT or its equivalent.
!

         IF (TRIM(LINE) .EQ. 'QUIT') EXIT
         IF (TRIM(LINE) .EQ. 'Q')    EXIT
         IF (TRIM(LINE) .EQ. 'EXIT') EXIT
         IF (TRIM(LINE) .EQ. 'OFF')  EXIT
         IF (TRIM(LINE) .EQ. 'BYE')  EXIT
         IF (TRIM(LINE) .EQ. 'STOP') EXIT
         IF (TRIM(LINE) .EQ. 'END')  EXIT

         PTR = 1

!     Loop for each element in the input line.

         DO
            IDX = INDEX(LINE(PTR:), ' ') + PTR - 1                                  ! look for the next space..
            IF (IDX .EQ. 0) IDX = LEN(LINE(PTR:))                                   ! ..or end of line
            SUBSTR = LINE(PTR:IDX-1)                                                ! get the current substring

            SELECT CASE (DOMAIN_MODE)
               CASE (1)
                  NUM_FLAG = ISREAL (SUBSTR, X)                                     ! convert to a real number, if possible
               CASE (2)
                  NUM_FLAG = ISCOMPLEX (SUBSTR, CX)                                 ! convert to a complex number, if possible
               CASE (3)
                  NUM_FLAG = ISRATIONAL (SUBSTR, RN, RD)                            ! convert to a rational number, if possible
            END SELECT

            IF (NUM_FLAG) THEN                                                      ! if a number, then put it on the stack
               SELECT CASE (DOMAIN_MODE)
                  CASE (1)
                     CALL PUSH_STACK (X)                                            ! push real number onto real stack
                  CASE (2)
                     CALL CPUSH_STACK (CX)                                          ! push complex number onto complex stack
                  CASE (3)
                     CALL RPUSH_STACK (RN, RD)                                      ! push rational number onto rational stack
               END SELECT
            ELSE                                                                    ! else it's an operator
               CALL EVAL (SUBSTR)                                                   ! evaluate operator
            END IF

            PTR = IDX + 1                                                           ! update line pointer
            IF (LEN_TRIM(LINE(PTR:)) .EQ. 0) EXIT                                   ! exit if at end of line
         END DO


!     Print X register.

         SELECT CASE (DOMAIN_MODE)
            CASE (1)
               CALL PRINTX(STACK(1), NUMSTR)                                        ! format REAL X
            CASE (2)
               CALL CPRINTX(CSTACK(1), NUMSTR)                                      ! format COMPLEX X
            CASE (3)
               CALL RPRINTX(RNSTACK(1), RDSTACK(1), NUMSTR)                         ! format RATIONAL X
         END SELECT

         print '(3X,A)', TRIM(NUMSTR)                                  ! print X

      END DO

      END PROGRAM RPN



