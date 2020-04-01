project('RPNcalc', 'fortran',
  default_options : ['default_library=static', 'warning_level=1', 'buildtype=release'],
  meson_version: '>=0.51',
  version: '1.2.1')

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_global_arguments('-Wno-all', '-fimplicit-none', language : 'fortran')
endif

subdir('src')

rpncalc_exe = executable('rpncalc', rpncalc_src,
  link_with: [reg, funcs, assert, hyper],
  install : true)

# tests
subdir('src/tests')