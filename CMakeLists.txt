cmake_minimum_required(VERSION 3.13)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(RPNcalc Fortran)
enable_testing()

include(cmake/compilers.cmake)

add_executable(rpncalc)
target_link_libraries(rpncalc PRIVATE reg funcs)
target_compile_options(rpncalc PRIVATE ${FFLAGS})

add_library(reg)
target_compile_options(reg PRIVATE ${FFLAGS})
target_link_libraries(reg PRIVATE assert)

#--- fucntions (usable in other programs too)
add_library(funcs)
target_link_libraries(funcs PRIVATE assert reg hyper)
target_compile_options(funcs PRIVATE ${FFLAGS})

add_library(hyper)
target_link_libraries(hyper PRIVATE reg)
target_compile_options(hyper PRIVATE ${FFLAGS})
if(f08hyper)
set_target_properties(hyper PROPERTIES
                        COMPILE_DEFINITIONS F08HYPER)
endif()

add_subdirectory(src)

#--- tests
add_executable(testcalc tests/test.f90)
target_link_libraries(testcalc assert funcs)
target_compile_options(testcalc PRIVATE ${FFLAGS})
add_test(NAME RPNfunc COMMAND testcalc)

#--- errors
add_library(assert tests/assert.f90)
target_compile_options(assert PRIVATE ${FFLAGS})

find_package(Python3)
if(Python3_FOUND)
  add_test(NAME UI COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_ui.py $<TARGET_FILE:rpncalc>)

  add_test(NAME Value COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_value.py $<TARGET_FILE:rpncalc>)
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
