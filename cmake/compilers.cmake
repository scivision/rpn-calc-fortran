set(CMAKE_CONFIGURATION_TYPES "Release;Debug" CACHE STRING "Build type selections" FORCE)


if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-mtune=native)

  string(APPEND CMAKE_Fortran_FLAGS " -fimplicit-none")
  #-Wall #-Wextra  -Warray-temporaries -Werror=array-bounds

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fcheck=all -fexceptions -ffpe-trap=invalid,zero,overflow -finit-real=nan -Wconversion")

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -warn -fpe0 -traceback")# -debug extended -check all)

  if(WIN32)
    add_compile_options(/arch:native)
    string(APPEND CMAKE_Fortran_FLAGS " /warn:declarations")
  else()
    add_compile_options(-march=native)
    string(APPEND CMAKE_Fortran_FLAGS " -warn declarations")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  string(APPEND CMAKE_Fortran_FLAGS " -C -Mdclchk")
endif()
