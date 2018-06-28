MODULE GLOBAL
use,intrinsic:: iso_fortran_env, only: wp=>real64

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: VERSION = '1.01d'

INTEGER, PARAMETER :: STACK_SIZE = 4                                          ! stack size
INTEGER, PARAMETER :: REG_SIZE = 10                                           ! number of storage registers
DOUBLE PRECISION, DIMENSION (STACK_SIZE) :: STACK                             ! real stack
DOUBLE PRECISION, DIMENSION (0:REG_SIZE-1) :: REG                             ! real storage registers
COMPLEX(wp), DIMENSION (STACK_SIZE) :: CSTACK                             ! complex stack
COMPLEX(wp), DIMENSION (0:REG_SIZE-1) :: CREG                             ! complex storage registers
INTEGER, DIMENSION (STACK_SIZE) :: RNSTACK, RDSTACK                           ! rational stack
INTEGER, DIMENSION (0:REG_SIZE-1) :: RNREG, RDREG                             ! rational storage registers
DOUBLE PRECISION :: LASTX                                                     ! real LAST X register
COMPLEX(wp) :: CLASTX                                                     ! complex LAST X register
INTEGER :: RNLASTX, RDLASTX                                                   ! rational LAST X register
DOUBLE PRECISION :: NN, SUMX, SUMX2, SUMY, SUMY2, SUMXY                       ! real summation registers
COMPLEX(wp) :: CNN, CSUMX, CSUMX2, CSUMY, CSUMY2, CSUMXY                  ! complex summation registers
INTEGER :: RNNN, RNSUMX, RNSUMX2, RNSUMY, RNSUMY2, RNSUMXY                    ! rational summation registers (num.)
INTEGER :: RDNN, RDSUMX, RDSUMX2, RDSUMY, RDSUMY2, RDSUMXY                    ! rational summation registers (den.)
DOUBLE PRECISION :: ANGLE_FACTOR, FRACTOL
INTEGER :: ANGLE_MODE, DISP_MODE, DISP_DIGITS, BASE_MODE, DOMAIN_MODE, &
   FRACTION_MODE

INTEGER, PARAMETER :: INITIAL_ANGLE_MODE = 1                                  ! 1=deg 2=rad 3=grad 4=rev
INTEGER, PARAMETER :: INITIAL_DISP_MODE = 4                                   ! 1=fix, 2=sci, 3=eng, 4=all
INTEGER, PARAMETER :: INITIAL_DISP_DIGITS = 4
INTEGER, PARAMETER :: INITIAL_DOMAIN_MODE = 1                                 ! 1=real, 2=complex, 3=rational
INTEGER, PARAMETER :: INITIAL_BASE_MODE = 10                                  ! 2=bin, 8=oct, 10=dec, 16=hex
INTEGER, PARAMETER :: INITIAL_FRACTION_MODE = 1                               ! 1=improper, 2=mixed
DOUBLE PRECISION, PARAMETER :: INITIAL_FRACTOL = 1.0D-4                       ! tolerance for decimal to fraction conversion

END MODULE GLOBAL
