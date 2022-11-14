# Netlib Specfun (vendored)

We have copied in this specfun/ directory select functions from [Netlib Specfun](https://netlib.org/specfun/).
In some cases we have modernized the Fortran code without changing the numeric output.
This includes:

* putting each procedure in a Fortran 90 module
* enabling "implicit none"
* using "intent" property
* using "elemental" property
