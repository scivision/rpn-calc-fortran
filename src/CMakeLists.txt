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
  
  if (NOT CYGWIN AND NOT WIN32)
    list(APPEND FFLAGS -fstack-protector-all)
  endif()

  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 8.1)
    list(APPEND FFLAGS -std=f2018)
  else()
    list(APPEND FFLAGS -std=f2008ts)
  endif()
  
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  list(APPEND FFLAGS -warn -fpe0 -traceback)# -debug extended -check all)
endif()

add_executable(rpncalc eval.f90 ui.f90 calc.f90)
target_link_libraries(rpncalc PRIVATE reg funcs)
target_compile_options(rpncalc PRIVATE ${FFLAGS})

add_library(reg reg.f90)
target_compile_options(reg PRIVATE ${FFLAGS})
target_link_libraries(reg PRIVATE assert)

add_library(funcs funcs.f90 bessel.f90 trig.f90 rat.f90 hyper.f90 stats.f90 gamma.f90)
target_link_libraries(funcs PRIVATE assert reg)
target_compile_options(funcs PRIVATE ${FFLAGS})

#--- tests
add_executable(testcalc tests/test.f90)
target_link_libraries(testcalc assert funcs)
target_compile_options(testcalc PRIVATE ${FFLAGS})
add_test(NAME RPNfunc COMMAND testcalc)

add_library(assert tests/assert.f90 tests/error2008.f90)
target_compile_options(assert PRIVATE ${FFLAGS})

if(UNIX)
  add_test(NAME UIrpn COMMAND bash -c "echo -e '2 5 * \n q' | ./rpncalc")
  add_test(NAME RPNinput COMMAND "./test.sh" 
           WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/../tests)
endif()

#--- install
if(UNIX AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
endif()

install(TARGETS rpncalc
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
