cmake_minimum_required(VERSION 3.14...3.21)

project(RPNcalc
  LANGUAGES Fortran
  VERSION 1.2.1
  HOMEPAGE_URL https://github.com/scivision/rpn-calc-fortran)

include(CTest)

include(cmake/compilers.cmake)

add_executable(rpncalc)
target_link_libraries(rpncalc PRIVATE assert hyper reg funcs)
install(TARGETS rpncalc)

#--- functions (usable in other programs too)
add_library(assert OBJECT src/tests/assert.f90)
target_include_directories(assert INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
set_target_properties(assert PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

add_subdirectory(src)
set(_targs assert rpncalc funcs hyper reg)

if(BUILD_TESTING)
  add_subdirectory(src/tests)
  list(APPEND _targs testcalc)
endif(BUILD_TESTING)

#--- properties
foreach(t ${_targs})
target_include_directories(${t} INTERFACE
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include  # IBM XL
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
set_target_properties(${t} PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
endforeach()

#--- install

install(FILES ${PROJECT_SOURCE_DIR}/README.md
  DESTINATION share/docs/${PROJECT_NAME})
