set(CMAKE_CONFIGURATION_TYPES "Release;Debug" CACHE STRING "Build type selections" FORCE)


if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-mtune=native)

  string(APPEND CMAKE_Fortran_FLAGS " -fimplicit-none")
  #-Wall #-Wextra  -Warray-temporaries -Werror=array-bounds

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fcheck=all -fexceptions -ffpe-trap=invalid,zero,overflow -finit-real=nan -Wconversion")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -warn -fpe0 -traceback")# -debug extended -check all)

  if(WIN32)
    add_compile_options(/QxHost)
    string(APPEND CMAKE_Fortran_FLAGS " /warn:declarations")
  else()
    add_compile_options(-xHost)
    string(APPEND CMAKE_Fortran_FLAGS " -warn declarations")
  endif()
endif()
