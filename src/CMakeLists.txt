cmake_policy(SET CMP0076 NEW)

target_sources(rpncalc PRIVATE help.f90 eval.f90 ui.f90 calc.f90)

target_sources(reg PRIVATE reg.f90)

target_sources(funcs PRIVATE funcs.f90
            bessel.f90 rjbesl.f rybesl.f i0.f i1.f k0.f k1.f ribesl.f rkbesl.f
            trig.f90 stats.f90 gamma.f90)

target_sources(hyper PRIVATE hyper.f90 rat.f90)