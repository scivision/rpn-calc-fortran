module stats

use assert, only: wp

implicit none (type, external)

contains

!***********************************************************************************************************************************
!  CNR
!
!  Combinations of N things taken R at a time.
!***********************************************************************************************************************************

elemental real(wp) FUNCTION CNR (N,R) RESULT (Y)

INTEGER, INTENT(IN) :: N, R
INTEGER :: I, J

Y = 1
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

Y = 1
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
use rat, only: rmul, rsub, rdiv

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


end module stats
