project('RPNcalc', 'fortran',
  default_options : ['default_library=static', 'warning_level=1'])

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

subdir('src')

assert = library('assert', 'tests/assert.F90',
  fortran_args: F08errstop)

reg = library('reg', 'src/reg.f90', link_with: assert)

hyper = library('hyper', 'src/hyper.F90', 'src/rat.f90',
  link_with: [assert, reg],
  fortran_args: F08hyper)

#--- functions (usable in other programs too)
funcs = library('funcs', funcs_src, link_with: [assert, reg, hyper])

rpncalc_exe = executable('rpncalc', rpncalc_src,
  link_with: [reg, funcs, assert, hyper],
  install : true)

#--- tests
testcalc_exe = executable('testcalc', 'tests/test.f90',
  link_with: [assert, funcs, reg, hyper])
test('RPNfunc', testcalc_exe)

testUI = find_program('tests/test_ui.py')
test('UI', testUI, args: rpncalc_exe)

tester = find_program('tests/test_value.py')
test('Values', tester, args: rpncalc_exe)