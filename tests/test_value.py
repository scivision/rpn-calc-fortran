#!/usr/bin/env python
import subprocess
import sys
from pathlib import Path

try:
    exe = Path(sys.argv[1])
except IndexError:
    raise ValueError('Must specify executable to run')

if not exe.is_file():
    raise FileNotFoundError(exe)

R = Path(__file__).parent

ref = (R/'test.asc').read_text()

ret = subprocess.check_output(str(exe), input=ref, universal_newlines=True, timeout=5)

match = 1
for line in ret.split('\n'):
    if '308 / 29' in line:
        match = 0
        break

raise SystemExit(match)
