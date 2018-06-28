# RPN Reverse Polish Notation Calculator -- in Fortran

Legacy code Author: David G. Simpson

## Build

    make
    
## Usage

    ./rpncalc
    
  Usage:        Start the program, then enter numbers and operations separated by blanks or carriage returns.
The contents of the X register will be printed after each carriage return.
For example:

            X

  2 3 +     5.0000
  6 *      30.0000
  SIN       0.5000
  
To exit the program, type any of: 
  
    QUIT, Q, EXIT, OFF, BYE, STOP, or END.

### Functions

Available operations:
!
+           Addition
-           Subtraction
*           Multiplication
/           Division
^           Exponentiation
\           Reciprocal
%           Percent
%CHG        Percent change
!           Factorial (= GAMMA(n+1))
!!          Double factorial
10X         10^x
2PI         2 * pi
2PII        2 * pi * i
2X          2^x
A0          Bohr radius (m)
ABS         Absolute value
ACOS        Inverse cosine
ACOSH       Inverse hyperbolic cosine
ACOT        Inverse cotangent
ACOT2       Inverse cotangent (2 args)
ACOTH       Inverse hyperbolic cotangent
ACOVERS     Inverse coversine
ACRD        Inverse chord (of Ptolemy)
ACSC        Inverse cosecant
ACSCH       Inverse hyperbolic cosecant
AEXSEC      Inverse exsecant
AHAV        Inverse haversine
ALL         ALL display mode
AMU         Atomic mass unit (kg)
AND         Logical AND
ARG         Argument of a complex number
ASEC        Inverse secant
ASECH       Inverse hyperbolic secant
ASIN        Inverse sine
ASINH       Inverse hyperbolic sine
ATAN        Inverse tangent
ATAN2       Inverse tangent (2 args)
ATANH       Inverse hyperbolic tangent
AU          Astronomical unit (m)
AVERS       Inverse versine
BESSELJ0    Bessel function of the first kind, order 0
BESSELJ1    Bessel function of the first kind, order 1
BESSELJ     Bessel function of the first kind, real order
BESSELY0    Bessel function of the second kind, order 0
BESSELY1    Bessel function of the second kind, order 1
BESSELY     Bessel function of the second kind, real order
BESSELI0    Modified Bessel function of the first kind, order 0
BESSELI1    Modified Bessel function of the first kind, order 1
BESSELI     Modified Bessel function of the first kind, real order
BESSELK0    Modified Bessel function of the second kind, order 0
BESSELK1    Modified Bessel function of the second kind, order 1
BESSELK     Modified Bessel function of the second kind, real order
BETA        Beta function
BIN         Binary mode
C           Speed of light in vacuum (m/s)
C>F         Celsius to Fahrenheit
CBRT        Cube root
CHS         Change sign
CLALL       Clear all
CLREG       Clear general registers
CLS         Clear summation registers
CLSTK       Clear stack
CLX         Clear X
CM>IN       Centimeter to inches
CNR         Combinations
COMPLEX     COMPLEX mode
CONJ        Complex conjugate
COS         Cosine
COSH        Hyperbolic cosine
COT         Cotangent
COTH        Hyperbolic cotangent
COVERS      Coversine
CRD         Chord (of Ptolemy)
CSC         Cosecant
CSCH        Hyperbolic cosecant
CUBE        Cube
D>F         Decimal to fraction
D>R         Degrees to radians
DEC         Decimal mode
DEFAULT     Restore default modes
DEG         Degrees mode
DIGAMMA     Digamma function
DUP         Duplicate X
ECHG        Elementary charge e (C)
ENG         Engineering notation
EPS0        Permittivity of free space (F/m)
ERF         Error function
ERFC        Complementary error function
EULER       Euler-Mascheroni constant
EXP         exp(x)
EXSEC       Exsecant
F>C         Fahrenheit to Celsius
FIX         Fix notation
FRAC        Fractional part
FRACTOL     Set fraction tolerance
G           Stamdard acceleration due to gravity (m/s^2)
GAL>L       Gallons to liters
GAMMA       Gamma function
GCD         Greatest common divisor
GOLDEN      Golden ratio
GRAD        Grads mode
GRAV        Gravitational constant G (m^3/kg s^2)
H           Planck constant (J s)
H>HMS       Hours to HMS
HAV         Haversine
HBAR        Planck constant (J s)
HEX         Hexadecimal mode
HMS>H       HMS to hours
HMS+        HMS add
HMS-        HMS subtract
HYPOT       Compute SQRT(X^2+Y^2)
HYPOT3      Compute SQRT(X^2+Y^2+Z^2)
I           i (imaginary unit)
IM          Imaginary component
IMPROPER    Improper fraction display mode
IN>CM       Inches to centimeters
INT         Integer part
INT/        Integer division
KB          Boltzmann constant (J/K)
KEPLER      Solves elliptical Kepler's equation (e,M -> E)
KG>LB       Kilograms to pounds
L>GAL       Liters to gallons
LASTX       Recall last X
LB>KG       Pounds to kilograms
LCM         Least common multiple
LN          Natural logarithm
LOG         Common logarithm
LOG2        Logarithm base 2
LR          Linear regression (leaves b in X, m in Y)
ME          Electron mass (kg)
MIXED       Mixed fraction display mode
MN          Neutron mass (kg)
MOD         Modulo
MODES       Print modes
MP          Proton mass (kg)
MU0         Permeability of free space (N/A^2)
MUB         Bohr magneton (A m^2)
MUN         Nuclear magneton (A m^2)
N           Number of points in summation
NA          Avogadro's number (mol^-1)
NOT         Logical NOT
OCT         Octal mode
OR          Logical OR
P>R         Polar to rectangular
PI          Pi
PNR         Permutations
PR          Print registers
PS          Print stack
PSUMS       Print sums
R           Roll stack down
R>D         Radians to degrees
R>P         Rectangular to polar
RAD         Radians mode
RAND        Random number
RATIONAL    Rational (fraction) mode
RCL         Recall
RCORR       Linear regression correlation coefficient
RE          Real component
REAL        REAL mode
REARTH      Earth radius (m)
REDUCE      Reduce an angle
RESET       Reset calculator to initial state
REV         Revs mode
RGAS        Gas constant (J/mol K)
RI          Exchange real and imaginary parts
ROUND       Round to integer
RUP         Roll stack up
RZETA       Riemann zeta function
S           Summation
S-          Delete summation
SCI         Scientific notation
SEC         Secant
SECH        Hyperbolic secant
SEED        Store random number seed
SGN         Signum
SIN         Sine
SINC        Sine cardinal (sinc) function
SINH        Hyperbolic sine
SINHC       Sinhc function
SQR         Square
SQRT        Square root
STEFAN      Stefan-Boltzmann constant (W/m^2 K^4)
STO         Store
SUMX        Summation of X
SUMX2       Summation of X^2
SUMXY       Summation of XY
SUMY        Summation of Y
SUMY2       Summation of Y^2
TAN         Tangent
TANC        Tanc function
TANH        Hyperbolic tangent
TANHC       Tanhc function
TIME        Current date and time
VER         Print software version
VERS        Versine
X^          Linear estimate X
XMEAN       Mean of X
XOR         Logical XOR
XRT         X root of Y
XS          Sample standard deviation of X
XSIG        Population standard deviation of X
XY          X-Y exchange
Y^          Linear estimate Y
YMEAN       Mean of Y
YS          Sample standard deviation of Y
YSIG        Population standard deviation of Y
!
Possible future operations:
!
?           Incomplete gamma functions (upper and lower)
?           Incomplete beta function
?           Jinc function
?           Spherical Bessel functions j, n
?           Legendre functions Pnm, Qnm (various normalizations)
?           Legendre polynomials
?           Elliptic integrals
?           Jacobi elliptic functions sn, cn, dn
?           Jacobi amplitude function am
?           Exponential integrals
?           Hypergeometric functions
?           Hermite polynomials
