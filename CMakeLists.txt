cmake_minimum_required(VERSION 3.19)

file(READ ${CMAKE_CURRENT_LIST_DIR}/codemeta.json _j)
string(JSON PROJECT_VERSION GET ${_j} version)

project(RPNcalc
LANGUAGES Fortran
VERSION ${PROJECT_VERSION}
HOMEPAGE_URL https://github.com/scivision/rpn-calc-fortran)

include(CTest)

include(cmake/compilers.cmake)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

add_executable(rpncalc)
target_include_directories(rpncalc INTERFACE
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)
target_link_libraries(rpncalc PRIVATE assert hyper reg funcs)

#--- functions (usable in other programs too)
add_library(assert OBJECT tests/assert.f90)
target_include_directories(assert INTERFACE
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)

add_subdirectory(src)

if(BUILD_TESTING)
  find_package(Python COMPONENTS Interpreter)

  if(Python_FOUND AND NOT DEFINED Scipy_VERSION)
    execute_process(COMMAND ${Python_EXECUTABLE} -c "import scipy; print(scipy.__version__)"
    RESULT_VARIABLE ret
    OUTPUT_VARIABLE out
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if(ret EQUAL 0)
      set(Scipy_VERSION ${out} CACHE STRING "Scipy version")
    else()
      set(Scipy_VERSION 0 CACHE STRING "Scipy Not Found")
    endif()

    message(STATUS "Found Scipy ${Scipy_VERSION}")
  endif()

  add_subdirectory(tests)
endif(BUILD_TESTING)


#--- install
install(TARGETS rpncalc)

install(FILES ${PROJECT_SOURCE_DIR}/README.md
  DESTINATION share/docs/${PROJECT_NAME})
