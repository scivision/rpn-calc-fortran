! main program for Fortran 2008 RPN calculator

!---- (historical notes) -----------
!  Programmer:   David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  20771
!  Date:         December 28, 2005
!-----------------------------------


      PROGRAM RPN
      use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit
      USE GLOBAL
      use funcs, only:  isrational, isreal, iscomplex, toUpper
      use stackops, only: printx, push_stack
      use evals, only: eval
      IMPLICIT NONE

      real(wp), PARAMETER :: PI = 4._wp * atan(1._wp)
      real(wp), PARAMETER :: TWOPI = 2*pi
      INTEGER :: I, IDX, IERR, DEL, PTR, RN, RD
      real(wp) :: X
      COMPLEX(wp) :: CX
      CHARACTER(300) :: LINE, SUBSTR
      CHARACTER(100) :: FMTSTR, NUMSTR
      LOGICAL :: NUM_FLAG


      print *, 'Fortran 2008  RPN Calculator, Version '//VERSION

!     Initialize data.

      call init_stack()

      DEL = IACHAR('a') - IACHAR('A')                                               ! find ASCII position diff between 'A' and 'a'

      STACK = 0._wp                                                                 ! clear the REAL stack
      REG = 0._wp                                                                   ! clear the REAL registers
      LASTX = 0._wp                                                                 ! clear the REAL LAST X register

      NN = 0._wp                                                                    ! clear the REAL summation registers
      SUMX = 0._wp
      SUMX2 = 0._wp
      SUMY = 0._wp
      SUMY2 = 0._wp
      SUMXY = 0._wp

      CSTACK = (0._wp,0._wp)                                                        ! clear the COMPLEX stack
      CREG = (0._wp,0._wp)                                                          ! clear the COMPLEX registers
      CLASTX = (0._wp,0._wp)                                                        ! clear the COMPLEX LAST X register

      CNN = (0._wp,0._wp)                                                           ! clear the COMPLEX summation registers
      CSUMX = (0._wp,0._wp)
      CSUMX2 = (0._wp,0._wp)
      CSUMY = (0._wp,0._wp)
      CSUMY2 = (0._wp,0._wp)
      CSUMXY = (0._wp,0._wp)

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
            ANGLE_FACTOR = PI/180._wp
         CASE (2)                                                                   ! rad
            ANGLE_FACTOR = 1._wp
         CASE (3)                                                                   ! grad
            ANGLE_FACTOR = PI/200._wp
         CASE (4)                                                                   ! rev
            ANGLE_FACTOR = TWOPI
      END SELECT

      DISP_MODE = INITIAL_DISP_MODE                                                 ! set modes
      DISP_DIGITS = INITIAL_DISP_DIGITS
      DOMAIN_MODE = INITIAL_DOMAIN_MODE
      BASE_MODE = INITIAL_BASE_MODE
      FRACTION_MODE = INITIAL_FRACTION_MODE

      FRACTOL = INITIAL_FRACTOL                                                     ! set decimal-to-fraction tolerance

!     call random_init()   ! Fortran 2018 + the following line
      CALL RANDOM_SEED()                                                           ! init random number generator

! -----  Main loop.

      DO                                                                            ! loop once for each input line
         WRITE(stdout,'(A)', ADVANCE='NO') '  ? '
         READ (stdin,'(A132)', iostat=ierr) LINE
         if (ierr<0) stop  ! Ctrl D was pressed

!     Convert the input line to all uppercase, removing leftmost blanks

         LINE = toUpper(ADJUSTL(LINE))
         
!     Search for QUIT 'Q'

         IF (TRIM(LINE) == 'Q') exit

         PTR = 1

!     Loop for each element in the input line.

         DO
            IDX = INDEX(LINE(PTR:), ' ') + PTR - 1                                  ! look for the next space..
            IF (IDX .EQ. 0) IDX = LEN(LINE(PTR:))                                   ! ..or end of line
            SUBSTR = LINE(PTR:IDX-1)                                                ! get the current substring

            SELECT CASE (DOMAIN_MODE)
               CASE (1)
                  NUM_FLAG = ISREAL(SUBSTR, X)                                     ! convert to a real number, if possible
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
                     CALL push_stack(CX)                                          ! push complex number onto complex stack
                  CASE (3)
                     CALL push_stack(RN, RD)                                      ! push rational number onto rational stack
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
               CALL PRINTX(CSTACK(1), NUMSTR)                                      ! format COMPLEX X
            CASE (3)
               CALL PRINTX(RNSTACK(1), RDSTACK(1), NUMSTR)                         ! format RATIONAL X
         END SELECT

         print '(3X,A)', TRIM(NUMSTR)                                  ! print X

      END DO

      END PROGRAM RPN

