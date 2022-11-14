add_library(reg OBJECT reg.f90)
target_link_libraries(reg PRIVATE assert)

add_library(funcs OBJECT funcs.f90
bessel.f90 trig.f90 stats.f90
)

foreach(s IN ITEMS gamma.f90 rjbesl.f rybesl.f ribesl.f rkbesl.f i0.f i1.f k0.f k1.f)
  target_sources(funcs PRIVATE specfun/${s})
endforeach()
target_link_libraries(funcs PRIVATE assert reg hyper)

add_library(hyper OBJECT hyper.f90 rat.f90)
target_link_libraries(hyper PRIVATE reg assert)
