function(test_stdin tgt txt)

execute_process(
COMMAND ${tgt}
INPUT_FILE ${txt}
TIMEOUT 10
RESULT_VARIABLE err)

if(NOT err EQUAL 0)
  message(FATAL_ERROR "${tgt} ${err}")
endif()

endfunction(test_stdin)


test_stdin(${tgt} ${txt})
