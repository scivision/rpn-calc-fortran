if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-fimplicit-none -Werror=line-truncation
  "$<$<CONFIG:Debug>:-fcheck=all;-fexceptions;-ffpe-trap=invalid,zero,overflow;-finit-real=nan;-Wconversion>"
  )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  add_compile_options(
  -traceback
  "$<$<CONFIG:Debug>:-warn;-fpe0>"
  )
endif()

include(GNUInstallDirs)
