#--- tests
add_executable(testcalc test.f90)
target_link_libraries(testcalc assert funcs hyper reg)
add_test(NAME RPNfunc COMMAND $<TARGET_FILE:testcalc>)
set_tests_properties(RPNfunc PROPERTIES TIMEOUT 10)

#--- errors
find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
add_test(NAME UI
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_ui.py $<TARGET_FILE:rpncalc>)
set_tests_properties(UI PROPERTIES TIMEOUT 10)

add_test(NAME Value
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_value.py $<TARGET_FILE:rpncalc>)
set_tests_properties(Value PROPERTIES TIMEOUT 10)
endif()
