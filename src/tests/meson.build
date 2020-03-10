#--- tests
testcalc_exe = executable('testcalc', 'test.f90',
  link_with: [assert, funcs, reg, hyper])
test('RPNfunc', testcalc_exe)

testUI = find_program('test_ui.py')
test('UI', testUI, args: rpncalc_exe.full_path())

tester = find_program('test_value.py')
test('Values', tester, args: rpncalc_exe.full_path())