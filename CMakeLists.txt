cmake_minimum_required(VERSION 3.14...3.25)

project(RPNcalc
LANGUAGES Fortran
VERSION 1.4.1
)

enable_testing()

option(${PROJECT_NAME}_BUILD_TESTING "build tests" on)

include(cmake/compilers.cmake)

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()


set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

add_executable(rpncalc app/main.f90 src/help.f90 src/eval.f90 src/ui.f90)
target_include_directories(rpncalc PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_link_libraries(rpncalc PRIVATE assert hyper reg funcs)

#--- functions (usable in other programs too)
add_library(assert OBJECT src/assert.F90)
target_include_directories(assert PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_compile_definitions(assert PRIVATE $<$<NOT:$<BOOL:${f03ieee}>>:NO_IEEE>)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  find_package(Python COMPONENTS Interpreter)

  if(Python_FOUND AND NOT DEFINED Scipy_ok)
    execute_process(COMMAND ${Python_EXECUTABLE} -c "import scipy; print('SciPy', scipy.__version__)"
    ERROR_VARIABLE err
    RESULT_VARIABLE ret
    )
    message(VERBOSE "${ret} ${err}")
    if(ret EQUAL 0)
      set(Scipy_ok true CACHE BOOL "Scipy OK")
    else()
      set(Scipy_ok false CACHE BOOL "Scipy Not Found")
    endif()
  endif()

  add_subdirectory(test)
endif()


#--- install
install(TARGETS rpncalc)

install(FILES ${PROJECT_SOURCE_DIR}/README.md
  DESTINATION share/docs/${PROJECT_NAME})
