set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Build type selections" FORCE)
set(CMAKE_NMC_DEFAULT_BUILD_FILE_CONFIG "Release" CACHE STRING "Default Build type" FORCE)


if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-march=native)

  string(APPEND CMAKE_Fortran_FLAGS " -fimplicit-none")
  #-Wall #-Wextra  -Warray-temporaries -Werror=array-bounds

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fcheck=all -fexceptions -ffpe-trap=invalid,zero,overflow -finit-real=nan -Wconversion")

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -warn -fpe0 -traceback")# -debug extended -check all)

  if(WIN32)
    string(APPEND CMAKE_Fortran_FLAGS " /warn:declarations")
  else()
    string(APPEND CMAKE_Fortran_FLAGS " -warn declarations")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  string(APPEND CMAKE_Fortran_FLAGS " -C -Mdclchk")
endif()
