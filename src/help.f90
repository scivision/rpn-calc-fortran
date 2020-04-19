module help
use, intrinsic:: iso_fortran_env, only: stderr=>output_unit

implicit none (type, external)

contains

subroutine print_help()
! Fortran doesn't know the location of its executable, making it interested to read a help file in a relative location.

print *, achar(13),'See README.md for complete documentation'
print *,'https://github.com/scivision/rpn-calc-fortran',new_line('')

print *,'Mode calculator is in:   MODES'
print *,'Stack:  R - roll down     U - roll up      D - drop'
print *,'        CLALL - clear all     CLS - Clear stack'
print *,'Number systems:     REAL     COMPLEX     RATIONAL'
print *,'Angular systems:    DEG     RAD     GRAD'
print *,'Number base:  BIN    DEC    HEX    OCT'
print *,'Register memory:  STOx   RCLx   where x ~ 0..9'

end subroutine print_help


end module help
