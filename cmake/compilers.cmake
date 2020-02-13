if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  set(CMAKE_Fortran_FLAGS "-march=native -fimplicit-none ")
  #-Wall #-Wextra  -Warray-temporaries -Werror=array-bounds

  set(CMAKE_Fortran_FLAGS_DEBUG "-fcheck=all -fexceptions -ffpe-trap=invalid,zero,overflow -finit-real=nan -Wconversion ")

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(CMAKE_Fortran_FLAGS_DEBUG "FFLAGS -warn -fpe0 -traceback ")# -debug extended -check all)

  if(WIN32)
    set(CMAKE_Fortran_FLAGS "/warn:declarations ")
  else()
    set(CMAKE_Fortran_FLAGS "-warn declarations ")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set(CMAKE_Fortran_FLAGS "-C -Mdclchk ")
endif()
