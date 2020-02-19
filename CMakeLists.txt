cmake_minimum_required(VERSION 3.13)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(RPNcalc
  LANGUAGES Fortran
  VERSION 1.2.0
  HOMEPAGE_URL https://github.com/scivision/rpn-calc-fortran)
enable_testing()

include(cmake/compilers.cmake)

add_executable(rpncalc)
target_link_libraries(rpncalc PRIVATE assert hyper reg funcs)
install(TARGETS rpncalc
  RUNTIME DESTINATION bin)

#--- functions (usable in other programs too)
add_subdirectory(src)

#--- tests
add_executable(testcalc tests/test.f90)
target_link_libraries(testcalc assert funcs hyper reg)
add_test(NAME RPNfunc COMMAND testcalc)

#--- errors
add_library(assert OBJECT tests/assert.f90)
target_include_directories(assert INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
set_target_properties(assert PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  add_test(NAME UI COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_ui.py $<TARGET_FILE:rpncalc>)

  add_test(NAME Value COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_value.py $<TARGET_FILE:rpncalc>)
endif()

#--- properties
foreach(t assert testcalc rpncalc funcs hyper reg)
target_include_directories(${t} INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
set_target_properties(${t} PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
endforeach()

#--- install

# Fortran does not have a sense of where it's executing from...
#install(FILES ${PROJECT_SOURCE_DIR}/../README.md
#        DESTINATION ${CMAKE_INSTALL_PREFIX}/../share/doc
#        PERMISSIONS OWNER_READ
#        RENAME help_rpncalc.txt)
