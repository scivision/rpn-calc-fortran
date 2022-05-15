funcs_src = files('funcs.f90', 'bessel.f90', 'rjbesl.f', 'rybesl.f', 'i0.f', 'i1.f', 'k0.f', 'k1.f', 'ribesl.f', 'rkbesl.f', 'trig.f90', 'stats.f90', 'gamma.f90')

rpncalc_src = files('help.f90', 'eval.f90', 'ui.f90', 'calc.f90')

assert = library('assert', '../tests/assert.f90')

reg = library('reg', 'reg.f90', link_with: assert)

hyper = library('hyper',
  sources : ['hyper.f90', 'rat.f90'],
  link_with: [assert, reg])

#--- functions (usable in other programs too)
funcs = library('funcs', funcs_src, link_with: [assert, reg, hyper])
