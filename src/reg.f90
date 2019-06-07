MODULE GLOBAL
use assert, only: wp

IMPLICIT NONE

CHARACTER(*), PARAMETER :: VERSION = '1.1.7'

INTEGER :: STACK_SIZE                                         ! stack size
INTEGER, PARAMETER :: REG_SIZE = 10                                           ! number of storage registers
real(wp), allocatable :: STACK(:)                           ! real stack
real(wp), DIMENSION (0:REG_SIZE-1) :: REG                             ! real storage registers
COMPLEX(wp), allocatable :: CSTACK(:)                           ! complex stack
COMPLEX(wp), DIMENSION (0:REG_SIZE-1) :: CREG                             ! complex storage registers
INTEGER, allocatable :: RNSTACK(:), RDSTACK(:)                      ! rational stack
INTEGER, DIMENSION (0:REG_SIZE-1) :: RNREG, RDREG                             ! rational storage registers
real(wp) :: LASTX                                                     ! real LAST X register
COMPLEX(wp) :: CLASTX                                                     ! complex LAST X register
INTEGER :: RNLASTX, RDLASTX                                                   ! rational LAST X register
real(wp) :: NN, SUMX, SUMX2, SUMY, SUMY2, SUMXY                       ! real summation registers
COMPLEX(wp) :: CNN, CSUMX, CSUMX2, CSUMY, CSUMY2, CSUMXY                  ! complex summation registers
INTEGER :: RNNN, RNSUMX, RNSUMX2, RNSUMY, RNSUMY2, RNSUMXY                    ! rational summation registers (num.)
INTEGER :: RDNN, RDSUMX, RDSUMX2, RDSUMY, RDSUMY2, RDSUMXY                    ! rational summation registers (den.)
real(wp) :: ANGLE_FACTOR, FRACTOL
INTEGER :: ANGLE_MODE, DISP_MODE, DISP_DIGITS, BASE_MODE, DOMAIN_MODE, &
   FRACTION_MODE

INTEGER, PARAMETER :: INITIAL_ANGLE_MODE = 1                                  ! 1=deg 2=rad 3=grad 4=rev
INTEGER, PARAMETER :: INITIAL_DISP_MODE = 4                                   ! 1=fix, 2=sci, 3=eng, 4=all
INTEGER, PARAMETER :: INITIAL_DISP_DIGITS = 4
INTEGER, PARAMETER :: INITIAL_DOMAIN_MODE = 1                                 ! 1=real, 2=complex, 3=rational
INTEGER, PARAMETER :: INITIAL_BASE_MODE = 10                                  ! 2=bin, 8=oct, 10=dec, 16=hex
INTEGER, PARAMETER :: INITIAL_FRACTION_MODE = 1                               ! 1=improper, 2=mixed
real(wp), PARAMETER :: INITIAL_FRACTOL = 1.0D-4                       ! tolerance for decimal to fraction conversion


contains


subroutine init_stack()

integer :: i
character(16) :: argv

stack_size = 4  ! default
call get_command_argument(1, argv, status=i)
if (i==0) read(argv,'(I2)') stack_size

if (stack_size < 2) stack_size=2  ! doesn't make sense to have only 1 level

allocate(stack(stack_size), cstack(stack_size), rdstack(stack_size), rnstack(stack_size))

end subroutine init_stack

END MODULE GLOBAL
