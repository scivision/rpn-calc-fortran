cmake_minimum_required(VERSION 3.7)
project(RPNcalc Fortran)

add_executable(rpncalc reg.f90 calc.f90)
target_link_libraries(rpncalc ui)
target_compile_options(rpncalc PRIVATE ${FFLAGS})

add_library(ui ui.f90)
target_link_libraries(ui PUBLIC funcs)
target_compile_options(ui PRIVATE ${FFLAGS})

add_library(funcs funcs.f90)
target_compile_options(funcs PRIVATE ${FFLAGS})

#----------------------------------
if(UNIX AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
endif()

install(TARGETS rpncalc
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
