cmake_minimum_required(VERSION 3.7)
project(RPNcalc Fortran)
enable_testing()

if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O3)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  list(APPEND FFLAGS -march=native #-Wall #-Wextra #-Wpedantic 
  -Werror=array-bounds -fbacktrace -fcheck=all
  -fexceptions -ffpe-trap=invalid,zero,overflow
  -finit-real=nan -Wconversion -fimplicit-none) #-Warray-temporaries
  
  if (UNIX AND NOT APPLE)
    list(APPEND FFLAGS -fstack-protector-all)
  endif()

  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 8)
    list(APPEND FFLAGS -std=f2018)
  endif()
  
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  list(APPEND FFLAGS -warn -fpe0 -traceback)# -debug extended -check all)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)
  link_libraries(-static-flang-libs)
  list(APPEND FFLAGS -Mallocatable=03)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
  list(APPEND FFLAGS -Mallocatable=03)
endif()

set(f08 GNU Intel)

add_executable(rpncalc help.f90 eval.f90 ui.f90 calc.f90)
target_link_libraries(rpncalc PRIVATE reg funcs)
target_compile_options(rpncalc PRIVATE ${FFLAGS})

add_library(reg reg.f90)
target_compile_options(reg PRIVATE ${FFLAGS})
target_link_libraries(reg PRIVATE assert)

#--- fucntions (usable in other programs too)
add_library(funcs funcs.f90 
            bessel.f90 rjbesl.f rybesl.f i0.f i1.f k0.f k1.f ribesl.f rkbesl.f
            trig.f90 rat.f90 stats.f90 gamma.f90)
target_link_libraries(funcs PRIVATE assert reg)
target_compile_options(funcs PRIVATE ${FFLAGS})

if(CMAKE_Fortran_COMPILER_ID IN_LIST f08)
  target_sources(funcs PRIVATE hyper.f90)
else()
  target_sources(funcs PRIVATE hyper2003.f90)
endif()

#--- tests
add_executable(testcalc tests/test.f90)
target_link_libraries(testcalc assert funcs)
target_compile_options(testcalc PRIVATE ${FFLAGS})
add_test(NAME RPNfunc COMMAND testcalc)

#--- errors
add_library(assert tests/assert.f90)
target_compile_options(assert PRIVATE ${FFLAGS})

if(CMAKE_Fortran_COMPILER_ID IN_LIST f08)
  target_sources(assert PRIVATE tests/error2008.f90)
else()
  target_sources(assert PRIVATE tests/error2003.f90)
endif()


if(UNIX)
  add_test(NAME UIrpn COMMAND bash -c "echo -e '2 5 * \n q' | ./rpncalc")
  
  add_test(NAME RPNinput 
           COMMAND bash -c "./rpncalc < ../tests/test.asc | tail -n1 | grep '308 / 29'")
endif()

#--- install
if(UNIX AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
endif()

install(TARGETS rpncalc
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
        
# Fortran does not have a sense of where it's executing from...
#install(FILES ${PROJECT_SOURCE_DIR}/../README.md
#        DESTINATION ${CMAKE_INSTALL_PREFIX}/../share/doc
#        PERMISSIONS OWNER_READ
#        RENAME help_rpncalc.txt)
