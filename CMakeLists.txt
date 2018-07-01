cmake_minimum_required(VERSION 3.7)
project(RPNcalc Fortran)
enable_testing()

add_executable(rpncalc calc.f90)
target_link_libraries(rpncalc ui)
target_compile_options(rpncalc PRIVATE ${FFLAGS})

add_library(ui ui.f90)
target_link_libraries(ui PUBLIC assert funcs)
target_compile_options(ui PRIVATE ${FFLAGS})

add_library(reg reg.f90)
target_compile_options(ui PRIVATE ${FFLAGS})
target_link_libraries(reg PRIVATE assert)

add_library(funcs funcs.f90)
target_link_libraries(funcs PRIVATE assert reg)
target_compile_options(funcs PRIVATE ${FFLAGS})

#------- tests
add_executable(testcalc tests/test.f90)
target_link_libraries(testcalc assert funcs)
target_compile_options(testcalc PRIVATE ${FFLAGS})
add_test(NAME RPNfunc COMMAND testcalc)

add_library(assert tests/assert.f90 tests/error2008.f90)
target_compile_options(assert PRIVATE ${FFLAGS})


#----------------------------------
if(UNIX AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
endif()

install(TARGETS rpncalc
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})