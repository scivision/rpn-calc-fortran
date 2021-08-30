#--- tests
testcalc_exe = executable('testcalc', 'test.f90',
  link_with: [assert, funcs, reg, hyper])
test('RPNfunc', testcalc_exe)
