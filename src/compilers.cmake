if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O3)
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  list(APPEND FFLAGS -march=native #-Wall #-Wextra #-Wpedantic  -Warray-temporaries
  -Werror=array-bounds -fimplicit-none )
  
  list(APPEND FFLAGS -fcheck=all -fexceptions -ffpe-trap=invalid,zero,overflow
  -finit-real=nan -Wconversion )
  
  if (UNIX AND NOT APPLE)
    list(APPEND FFLAGS -fstack-protector-all)
  endif()

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    list(APPEND FFLAGS -std=f2018)
  endif()
  
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  list(APPEND FFLAGS -warn -fpe0 -traceback)# -debug extended -check all)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)
  link_libraries(-static-flang-libs)
  list(APPEND FFLAGS -Mallocatable=03)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  list(APPEND FFLAGS -Mallocatable=03)
endif()

include(CheckFortranSourceCompiles)

check_fortran_source_compiles("
program a
use, intrinsic:: ieee_arithmetic
end" 
  f08ieee SRC_EXT f90)
  
if(NOT f08ieee)
  message(FATAL_ERROR "IEEE_arithmetic not supported by your compiler")
endif()

check_fortran_source_compiles("
program a 
print *,atanh((1.,1.))
end" 
f08hyper SRC_EXT f90)
                              
check_fortran_source_compiles("program a; error stop; end" 
  f08errorstop SRC_EXT f90)

