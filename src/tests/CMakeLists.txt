#--- tests
add_executable(testcalc test.f90)
target_link_libraries(testcalc assert funcs hyper reg)
add_test(NAME RPNfunc COMMAND $<TARGET_FILE:testcalc>)
set_tests_properties(RPNfunc PROPERTIES TIMEOUT 10)

#--- errors
add_library(assert OBJECT assert.f90)
target_include_directories(assert INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
set_target_properties(assert PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
add_test(NAME UI
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_ui.py $<TARGET_FILE:rpncalc>)
set_tests_properties(UI PROPERTIES TIMEOUT 10)

add_test(NAME Value
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_value.py $<TARGET_FILE:rpncalc>)
set_tests_properties(Value PROPERTIES TIMEOUT 10)
endif()