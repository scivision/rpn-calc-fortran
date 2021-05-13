cmake_minimum_required(VERSION 3.14...${CMAKE_VERSION})

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "default build type")
endif()

project(RPNcalc
  LANGUAGES Fortran
  VERSION 1.2.1
  HOMEPAGE_URL https://github.com/scivision/rpn-calc-fortran)

include(CTest)

include(cmake/compilers.cmake)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # will not take effect without FORCE
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "Install top-level directory" FORCE)
endif()

# this helps linters e.g. Visual Studio Intellicode work properly
set(CMAKE_EXPORT_COMPILE_COMMANDS on)

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
  DESTINATION share/doc)
