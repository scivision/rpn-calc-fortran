#--- tests
add_executable(testcalc test.f90)
target_link_libraries(testcalc PRIVATE assert funcs hyper reg)

add_test(NAME RPNfunc COMMAND testcalc)

add_test(NAME UI
COMMAND ${CMAKE_COMMAND} -Dtgt:FILEPATH=$<TARGET_FILE:rpncalc> -Dtxt:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/test_ui.asc -P ${PROJECT_SOURCE_DIR}/cmake/test_stdin.cmake)
set_tests_properties(UI PROPERTIES
PASS_REGULAR_EXPRESSION "10.0"
)

add_test(NAME Value
COMMAND ${CMAKE_COMMAND} -Dtgt:FILEPATH=$<TARGET_FILE:rpncalc> -Dtxt:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/test_value.asc -P ${PROJECT_SOURCE_DIR}/cmake/test_stdin.cmake)
set_tests_properties(Value PROPERTIES
PASS_REGULAR_EXPRESSION "308 / 29"
)

# add_test(NAME Bessel
# COMMAND ${CMAKE_COMMAND} -Dtgt:FILEPATH=$<TARGET_FILE:rpncalc> -Dtxt:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/test_value.asc -P ${PROJECT_SOURCE_DIR}/cmake/test_stdin.cmake)
# set_tests_properties(Value PROPERTIES
# PASS_REGULAR_EXPRESSION "308 / 29"
# )

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_tests_properties(${test_names} PROPERTIES TIMEOUT 10)
