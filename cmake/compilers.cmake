set(CMAKE_CONFIGURATION_TYPES "Release;Debug" CACHE STRING "Build type selections" FORCE)


if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-mtune=native -fimplicit-none -Werror=line-truncation
  "$<$<CONFIG:Debug>:-fcheck=all;-fexceptions;-ffpe-trap=invalid,zero,overflow;-finit-real=nan;-Wconversion>"
  )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  add_compile_options(
  "$<$<CONFIG:Debug>:-warn;-fpe0;-traceback>"
  "$<IF:$<BOOL:${WIN32}>,/QxHost,-xHost>"
  )
endif()
