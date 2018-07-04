module stackops
use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use global
use assert, only: wp, isclose
use funcs
implicit none

interface push_stack
  procedure push_stack_r, cpush_stack, rpush_stack
end interface push_stack

interface printx
  procedure printx_r, cprintx, rprintx
end interface 

complex(wp), parameter, private :: c0 = (0._wp, 0._wp)

contains

!***********************************************************************************************************************************
!  PUSH_STACK
!
!  Push a number onto the real stack.
!***********************************************************************************************************************************

SUBROUTINE PUSH_STACK_r(X)
real(wp), INTENT(IN) :: X

stack = cshift(stack, -1)

STACK(1) = X
END SUBROUTINE PUSH_STACK_r


!***********************************************************************************************************************************
!  push_stack
!
!  Push a number onto the complex stack.
!***********************************************************************************************************************************

SUBROUTINE CPUSH_STACK(X)
COMPLEX(wp), INTENT(IN) :: X

 cstack = cshift(cstack, -1)

 CSTACK(1) = X
END SUBROUTINE CPUSH_STACK


!***********************************************************************************************************************************
!  RPUSH_STACK
!
!  Push a number onto the rational stack.
!***********************************************************************************************************************************

SUBROUTINE RPUSH_STACK(RN,RD)
INTEGER, INTENT(IN) :: RN,RD

rnstack = cshift(rnstack, -1)
rdstack = cshift(rdstack, -1)

RNSTACK(1) = RN
RDSTACK(1) = RD

END SUBROUTINE RPUSH_STACK


!***********************************************************************************************************************************
!  DROP_STACK
!
!  Drop a number from the real stack.
!***********************************************************************************************************************************

SUBROUTINE DROP_STACK(N)
INTEGER, INTENT(IN) :: N

STACK(N:stack_size-1) = STACK(N+1:stack_size)

stack(stack_size) = 0._wp

END SUBROUTINE DROP_STACK


!***********************************************************************************************************************************
!  CDROP_STACK
!
!  Drop a number from the complex stack.
!***********************************************************************************************************************************

SUBROUTINE CDROP_STACK(N)
INTEGER, INTENT(IN) :: N

 CSTACK(N:stack_size-1) = CSTACK(N+1:stack_size)

 Cstack(stack_size) = 0._wp

END SUBROUTINE CDROP_STACK



!***********************************************************************************************************************************
!  RDROP_STACK
!
!  Drop a number from the rational stack.
!***********************************************************************************************************************************

SUBROUTINE RDROP_STACK (N)
INTEGER, INTENT(IN) :: N


RNSTACK(N:stack_size-1) = RNSTACK(N+1:stack_size)
RDSTACK(N:stack_size-1) = RDSTACK(N+1:stack_size)

RNstack(stack_size) = 0
RDstack(stack_size) = 0

END SUBROUTINE RDROP_STACK


!***********************************************************************************************************************************
!  PRINTX
!
!  Print a real number to a string.
!***********************************************************************************************************************************

SUBROUTINE PRINTX_r(X, NUMSTR)

real(wp), INTENT(IN) :: X
CHARACTER(100), INTENT(OUT) :: NUMSTR

real(wp) :: TMPX
CHARACTER(9) :: F1, F2, F3
character(1) :: F

write(F,'(I1)') DISP_DIGITS
F1 ='(4F15.'//F//')' 
F2 = '(5ES15.'//F//')'
F3 = '(5EN15.'//F//')'

IF (BASE_MODE == 10) THEN                                                   ! DEC mode
   SELECT CASE (DISP_MODE)
      CASE (1)                                                                ! print X (FIX)
         WRITE (NUMSTR, F1) X
         IF (INDEX(NUMSTR,'*') /= 0)  WRITE(NUMSTR, F2) X
         
         READ (NUMSTR, *) TMPX
         IF (.not.isclose(x, 0._wp) .AND. isclose(TMPX,0._wp)) WRITE (NUMSTR, F2) X      !   disp. underflow          
      CASE (2)                                                                ! print X (SCI)
         WRITE (NUMSTR, F2) X
      CASE (3)                                                                ! print X (ENG)
         WRITE (NUMSTR, F3) X
      CASE (4)                                                                ! print X (ALL)
         WRITE (NUMSTR, '(1PG23.15)') X
   END SELECT
ELSE
   SELECT CASE (BASE_MODE)
      CASE (2)                                                                ! print X (BIN) 
         WRITE (NUMSTR, '(B0)') INT(X)
      CASE (8)                                                                ! print X (OCT)
         WRITE (NUMSTR, '(O0)') INT(X)
      CASE (16)                                                               ! print X (HEX)
         WRITE (NUMSTR, '(Z0)') INT(X)
   END SELECT
END IF

END SUBROUTINE PRINTX_r

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

      IF (BASE_MODE == 10) THEN                                                   ! DEC mode
         SELECT CASE (DISP_MODE)
            CASE (1)                                                                ! print X (FIX)
               WRITE (UNIT=FMTSTR, FMT=800) DISP_DIGITS, DISP_DIGITS
  800          FORMAT ("(ES25.",I0,",SP,4X,F25.",I0,")")
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
               IF (INDEX(NUMSTR,'*') /= 0) THEN                                   !   disp. overflow
                  WRITE (UNIT=FMTSTR, FMT=810)  DISP_DIGITS, DISP_DIGITS
  810             FORMAT ("(EN25.",I0,",SP,4X,ES25.",I0,")")
                  WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
               END IF
               READ (UNIT=NUMSTR, FMT=*) TMPX
               IF (.not.isclose(x, C0) .AND. isclose(TMPX, C0)) THEN                     !   disp. underflow
                  WRITE (UNIT=FMTSTR, FMT=820) DISP_DIGITS, DISP_DIGITS
  820             FORMAT ("(EN25.",I0,",SP,4X,ES25.",I0,")")
                  WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
               END IF
            CASE (2)                                                                ! print X (SCI)
               WRITE (UNIT=FMTSTR, FMT=830) DISP_DIGITS, DISP_DIGITS
  830          FORMAT ("(ES25.",I0,",SP,4X,ES25.",I0,")")
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
            CASE (3)                                                                ! print X (ENG)
               WRITE (UNIT=FMTSTR, FMT=840) DISP_DIGITS, DISP_DIGITS
  840          FORMAT ("(EN25.",I0,",SP,4X,ES25.",I0,")")
               WRITE (UNIT=NUMSTR, FMT=FMTSTR) REAL(X, WP), AIMAG(X)
            CASE (4)                                                            ! print X (ALL)
               WRITE (UNIT=FMTSTR, FMT='(A)') '(1PG23.15,SP,4X,G23.15)'
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
      IF (RD == 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(B0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(B0,A3,B0)') RN,' / ', RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(B0,3X,B0,A3,B0)') A1, A2,' / ', A3
         END SELECT
      END IF
   CASE (8)                                                                   ! print X (OCT)
      IF (RD == 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(O0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(O0,A3,O0)') RN, ' / ',RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(O0,3X,O0,A3,O0)') A1, A2, ' / ', A3
         END SELECT
      END IF
   CASE (10)                                                                  ! print X (DEC)
      IF (RD == 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(I0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(I0,A3,I0)') RN,' / ', RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(I0,3X,I0,A3,I0)') A1, A2,' / ', A3
         END SELECT
      END IF
   CASE (16)                                                                  ! print X (HEX)
      IF (RD == 1) THEN
         WRITE (UNIT=NUMSTR, FMT='(Z0)') RN
      ELSE
         SELECT CASE (FRACTION_MODE)
            CASE (1)
               WRITE (UNIT=NUMSTR, FMT='(Z0,A3,Z0)') RN,' / ', RD
            CASE (2)
               CALL FRAC_TO_MIXED (RN, RD, A1, A2, A3)
               WRITE (UNIT=NUMSTR, FMT='(Z0,3X,Z0,A3,Z0)') A1, A2,' / ', A3
         END SELECT
      END IF
!         CASE (16)                                                                  ! print X (HEX)
!            IF (RD == 1) THEN
!               WRITE (UNIT=NUMSTR, FMT='(Z0)') RN
!            ELSE
!               WRITE (UNIT=NUMSTR, FMT='(Z0,3H / ,Z0)') RN, RD
!            END IF
END SELECT

END SUBROUTINE RPRINTX

end module stackops
