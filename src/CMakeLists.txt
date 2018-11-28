cmake_minimum_required(VERSION 3.0)
project(RPNcalc Fortran)
enable_testing()

include(compilers.cmake)

add_executable(rpncalc help.f90 eval.f90 ui.f90 calc.f90)
target_link_libraries(rpncalc PRIVATE reg funcs)
target_compile_options(rpncalc PRIVATE ${FFLAGS})

add_library(reg reg.f90)
target_compile_options(reg PRIVATE ${FFLAGS})
target_link_libraries(reg PRIVATE assert)

#--- fucntions (usable in other programs too)
add_library(funcs funcs.f90 
            bessel.f90 rjbesl.f rybesl.f i0.f i1.f k0.f k1.f ribesl.f rkbesl.f
            trig.f90 stats.f90 gamma.f90)
target_link_libraries(funcs PRIVATE assert reg hyper)
target_compile_options(funcs PRIVATE ${FFLAGS})

add_library(hyper hyper.F90 rat.f90)
target_link_libraries(hyper PRIVATE reg)
target_compile_options(hyper PRIVATE ${FFLAGS})
if(f08hyper)
set_target_properties(hyper PROPERTIES 
                        COMPILE_DEFINITIONS F08HYPER)
endif()
#--- tests
add_executable(testcalc tests/test.f90)
target_link_libraries(testcalc assert funcs)
target_compile_options(testcalc PRIVATE ${FFLAGS})
add_test(NAME RPNfunc COMMAND testcalc)

#--- errors
add_library(assert tests/assert.F90)
target_compile_options(assert PRIVATE ${FFLAGS})
if(f08errorstop)
  set_target_properties(assert PROPERTIES 
                        COMPILE_DEFINITIONS F08)
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
