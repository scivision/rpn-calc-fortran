add_library(reg OBJECT reg.f90)
target_link_libraries(reg PRIVATE assert)

add_library(funcs OBJECT funcs.f90
bessel.f90 rjbesl.f rybesl.f i0.f i1.f k0.f k1.f ribesl.f rkbesl.f
trig.f90 stats.f90 gamma.f90)
target_link_libraries(funcs PRIVATE assert reg hyper)

add_library(hyper OBJECT hyper.f90 rat.f90)
target_link_libraries(hyper PRIVATE reg assert)
