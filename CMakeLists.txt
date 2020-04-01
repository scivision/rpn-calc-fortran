cmake_minimum_required(VERSION 3.13)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(RPNcalc
  LANGUAGES Fortran
  VERSION 1.2.1
  HOMEPAGE_URL https://github.com/scivision/rpn-calc-fortran)
enable_testing()

include(cmake/compilers.cmake)

add_executable(rpncalc)
target_link_libraries(rpncalc PRIVATE assert hyper reg funcs)
install(TARGETS rpncalc
  RUNTIME DESTINATION bin)

#--- functions (usable in other programs too)
add_subdirectory(src)

#--- properties
foreach(t assert testcalc rpncalc funcs hyper reg)
target_include_directories(${t} INTERFACE
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include  # IBM XL
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
set_target_properties(${t} PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
endforeach()

#--- install

#install(FILES ${PROJECT_SOURCE_DIR}/../README.md
#        DESTINATION ${CMAKE_INSTALL_PREFIX}/../share/doc
#        PERMISSIONS OWNER_READ
#        RENAME help_rpncalc.txt)
