project('RPNcalc', 'fortran', default_options : ['default_library=static', 'warning_level=1'])

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_global_arguments('-Wno-all', '-fimplicit-none', language : 'fortran')
endif

F08errstop=[]
if fc.compiles('error stop; end')
  F08errstop='-DF08'
endif

F08hyper = []
if fc.links('complex :: x; asech(x); end')
  F08hyper = '-DF08HYPER'
endif

assert = library('assert', 'tests/assert.F90',
  fortran_args: F08errstop)

reg = library('reg', 'reg.f90', link_with: assert)

hyper = library('hyper', 'hyper.F90', 'rat.f90',
  link_with: [assert, reg],
  fortran_args: F08hyper)
  
#--- functions (usable in other programs too)
funcs = library('funcs', 'funcs.f90', 'bessel.f90', 'rjbesl.f', 'rybesl.f', 'i0.f', 'i1.f', 'k0.f', 'k1.f', 'ribesl.f', 'rkbesl.f', 'trig.f90', 'stats.f90', 'gamma.f90',
  link_with: [assert, reg, hyper])
  
rpncalc_exe = executable('rpncalc', 'help.f90', 'eval.f90', 'ui.f90', 'calc.f90',
  link_with: [reg, funcs, assert, hyper],
  install : true)

#--- tests
testcalc_exe = executable('testcalc', 'tests/test.f90',
  link_with: [assert, funcs, reg, hyper])
test('RPNfunc', testcalc_exe)

