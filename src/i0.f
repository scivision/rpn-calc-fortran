      module i0

      implicit none (type, external)
      private
      public:: besi0

      contains

      SUBROUTINE CALCI0(ARG,RESULT,JINT)
C--------------------------------------------------------------------
C
C This packet computes modified Bessel functions of the first kind
C   and order zero, I0(X) and EXP(-ABS(X))*I0(X), for real
C   arguments X.  It contains two function type subprograms, BESI0
C   and BESEI0, and one subroutine type subprogram, CALCI0.
C   The calling statements for the primary entries are
C
C                   Y=BESI0(X)
C   and
C                   Y=BESEI0(X)
C
C   where the entry points correspond to the functions I0(X) and
C   EXP(-ABS(X))*I0(X), respectively.  The routine CALCI0 is
C   intended for internal packet use only, all computations within
C   the packet being concentrated in this routine.  The function
C   subprograms invoke CALCI0 with the statement
C          CALL CALCI0(ARG,RESULT,JINT)
C   where the parameter usage is as follows
C
C      Function                     Parameters for CALCI0
C       Call              ARG                  RESULT          JINT
C
C     BESI0(ARG)    ABS(ARG) .LE. XMAX        I0(ARG)           1
C     BESEI0(ARG)    any real ARG        EXP(-ABS(ARG))*I0(ARG) 2
C
C   The main computation evaluates slightly modified forms of
C   minimax approximations generated by Blair and Edwards, Chalk
C   River (Atomic Energy of Canada Limited) Report AECL-4928,
C   October, 1974.  This transportable program is patterned after
C   the machine-dependent FUNPACK packet NATSI0, but cannot match
C   that version for efficiency or accuracy.  This version uses
C   rational functions that theoretically approximate I-SUB-0(X)
C   to at least 18 significant decimal digits.  The accuracy
C   achieved depends on the arithmetic system, the compiler, the
C   intrinsic functions, and proper selection of the machine-
C   dependent constants.
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants
C
C   beta   = Radix for the floating-point system
C   maxexp = Smallest power of beta that overflows
C   XSMALL = Positive argument such that 1.0 - X = 1.0 to
C            machine precision for all ABS(X) .LE. XSMALL.
C   XINF =   Largest positive machine number; approximately
C            beta**maxexp
C   XMAX =   Largest argument acceptable to BESI0;  Solution to
C            equation:
C               W(X) * (1+1/(8*X)+9/(128*X**2) = beta**maxexp
C            where  W(X) = EXP(X)/SQRT(2*PI*X)
C
C
C     Approximate values for some important machines are:
C
C                          beta       maxexp       XSMALL
C
C CRAY-1        (S.P.)       2         8191       3.55E-15
C Cyber 180/855
C   under NOS   (S.P.)       2         1070       3.55E-15
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)       2          128       2.98E-8
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)       2         1024       5.55D-17
C IBM 3033      (D.P.)      16           63       6.95D-18
C VAX           (S.P.)       2          127       2.98E-8
C VAX D-Format  (D.P.)       2          127       6.95D-18
C VAX G-Format  (D.P.)       2         1023       5.55D-17
C
C
C                               XINF          XMAX
C
C CRAY-1        (S.P.)       5.45E+2465     5682.810
C Cyber 180/855
C   under NOS   (S.P.)       1.26E+322       745.893
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)       3.40E+38         91.900
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)       1.79D+308       713.986
C IBM 3033      (D.P.)       7.23D+75        178.182
C VAX           (S.P.)       1.70D+38         91.203
C VAX D-Format  (D.P.)       1.70D+38         91.203
C VAX G-Format  (D.P.)       8.98D+307       713.293
C
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns XINF for BESI0 for ABS(ARG) .GT. XMAX.
C
C
C  Intrinsic functions required are:
C
C     ABS, SQRT, EXP
C
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C  Latest modification: June 7, 1988
C
C--------------------------------------------------------------------
      INTEGER I,JINT
CS    REAL
      DOUBLE PRECISION
     1       A,ARG,B,EXP40,FORTY,ONE,ONE5,P,PP,Q,QQ,RESULT,
     2       REC15,SUMP,SUMQ,TWO25,X,XINF,XMAX,XSMALL,XX
      DIMENSION P(15),PP(8),Q(5),QQ(7)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
CS    DATA ONE/1.0E0/,ONE5/15.0E0/,EXP40/2.353852668370199854E17/,
CS   1     FORTY/40.0E0/,REC15/6.6666666666666666666E-2/,
CS   2     TWO25/225.0E0/
CD    DATA ONE/1.0D0/,ONE5/15.0D0/,EXP40/2.353852668370199854D17/,
CD   1     FORTY/40.0D0/,REC15/6.6666666666666666666D-2/,
CD   2     TWO25/225.0D0/
C--------------------------------------------------------------------
C  Machine-dependent constants
C--------------------------------------------------------------------
CS    DATA XSMALL/2.98E-8/,XINF/3.40E38/,XMAX/91.9E0/
CD    DATA XSMALL/5.55D-17/,XINF/1.79D308/,XMAX/713.986D0/
C--------------------------------------------------------------------
C  Coefficients for XSMALL .LE. ABS(ARG) .LT. 15.0
C--------------------------------------------------------------------
CS    DATA  P/-5.2487866627945699800E-18,-1.5982226675653184646E-14,
CS   1        -2.6843448573468483278E-11,-3.0517226450451067446E-08,
CS   2        -2.5172644670688975051E-05,-1.5453977791786851041E-02,
CS   3        -7.0935347449210549190E+00,-2.4125195876041896775E+03,
CS   4        -5.9545626019847898221E+05,-1.0313066708737980747E+08,
CS   5        -1.1912746104985237192E+10,-8.4925101247114157499E+11,
CS   6        -3.2940087627407749166E+13,-5.5050369673018427753E+14,
CS   7        -2.2335582639474375249E+15/
CS    DATA  Q/-3.7277560179962773046E+03, 6.5158506418655165707E+06,
CS   1        -6.5626560740833869295E+09, 3.7604188704092954661E+12,
CS   2        -9.7087946179594019126E+14/
CD    DATA  P/-5.2487866627945699800D-18,-1.5982226675653184646D-14,
CD   1        -2.6843448573468483278D-11,-3.0517226450451067446D-08,
CD   2        -2.5172644670688975051D-05,-1.5453977791786851041D-02,
CD   3        -7.0935347449210549190D+00,-2.4125195876041896775D+03,
CD   4        -5.9545626019847898221D+05,-1.0313066708737980747D+08,
CD   5        -1.1912746104985237192D+10,-8.4925101247114157499D+11,
CD   6        -3.2940087627407749166D+13,-5.5050369673018427753D+14,
CD   7        -2.2335582639474375249D+15/
CD    DATA  Q/-3.7277560179962773046D+03, 6.5158506418655165707D+06,
CD   1        -6.5626560740833869295D+09, 3.7604188704092954661D+12,
CD   2        -9.7087946179594019126D+14/
C--------------------------------------------------------------------
C  Coefficients for 15.0 .LE. ABS(ARG)
C--------------------------------------------------------------------
CS    DATA PP/-3.9843750000000000000E-01, 2.9205384596336793945E+00,
CS   1        -2.4708469169133954315E+00, 4.7914889422856814203E-01,
CS   2        -3.7384991926068969150E-03,-2.6801520353328635310E-03,
CS   3         9.9168777670983678974E-05,-2.1877128189032726730E-06/
CS    DATA QQ/-3.1446690275135491500E+01, 8.5539563258012929600E+01,
CS   1        -6.0228002066743340583E+01, 1.3982595353892851542E+01,
CS   2        -1.1151759188741312645E+00, 3.2547697594819615062E-02,
CS   3        -5.5194330231005480228E-04/
CD    DATA PP/-3.9843750000000000000D-01, 2.9205384596336793945D+00,
CD   1        -2.4708469169133954315D+00, 4.7914889422856814203D-01,
CD   2        -3.7384991926068969150D-03,-2.6801520353328635310D-03,
CD   3         9.9168777670983678974D-05,-2.1877128189032726730D-06/
CD    DATA QQ/-3.1446690275135491500D+01, 8.5539563258012929600D+01,
CD   1        -6.0228002066743340583D+01, 1.3982595353892851542D+01,
CD   2        -1.1151759188741312645D+00, 3.2547697594819615062D-02,
CD   3        -5.5194330231005480228D-04/
C--------------------------------------------------------------------
      X = ABS(ARG)
      IF (X .LT. XSMALL) THEN
            RESULT = ONE
         ELSE IF (X .LT. ONE5) THEN
C--------------------------------------------------------------------
C  XSMALL .LE.  ABS(ARG)  .LT. 15.0
C--------------------------------------------------------------------
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
C--------------------------------------------------------------------
C  15.0  .LE.  ABS(ARG)
C--------------------------------------------------------------------
                  XX = ONE / X - REC15
                  SUMP = ((((((PP(1)*XX+PP(2))*XX+PP(3))*XX+PP(4))*XX+
     1                   PP(5))*XX+PP(6))*XX+PP(7))*XX+PP(8)
                  SUMQ = ((((((XX+QQ(1))*XX+QQ(2))*XX+QQ(3))*XX+
     1                   QQ(4))*XX+QQ(5))*XX+QQ(6))*XX+QQ(7)
                  RESULT = SUMP / SUMQ
                  IF (JINT .EQ. 2) THEN
                        RESULT = (RESULT - PP(1)) / SQRT(X)
                     ELSE
C--------------------------------------------------------------------
C  Calculation reformulated to avoid premature overflow
C--------------------------------------------------------------------
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
C--------------------------------------------------------------------
C  Return for ABS(ARG) .LT. XSMALL
C--------------------------------------------------------------------
      RETURN
C----------- Last line of CALCI0 -----------
      END subroutine calci0

CS    REAL
      DOUBLE PRECISION FUNCTION BESI0(X)
C--------------------------------------------------------------------
C
C This long precision subprogram computes approximate values for
C   modified Bessel functions of the first kind of order zero for
C   arguments ABS(ARG) .LE. XMAX  (see comments heading CALCI0).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION X, RESULT
C--------------------------------------------------------------------
      JINT=1
      CALL CALCI0(X,RESULT,JINT)
      BESI0=RESULT
      RETURN
C---------- Last line of BESI0 ----------
      END function besi0
CS    REAL
      DOUBLE PRECISION FUNCTION BESEI0(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the first kind of order zero
C   multiplied by EXP(-ABS(X)), where EXP is the
C   exponential function, ABS is the absolute value, and X
C   is any argument.
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION  X, RESULT
C--------------------------------------------------------------------
      JINT=2
      CALL CALCI0(X,RESULT,JINT)
      BESEI0=RESULT
      RETURN
C---------- Last line of BESEI0 ----------
      END function besei0

      end module i0
