#!/usr/bin/env python
import subprocess
import sys
from pathlib import Path
import shutil

exe = shutil.which(sys.argv[1])
if not exe:
    print('executable', sys.argv[1], 'not found', file=sys.stderr)
    raise SystemExit(77)

R = Path(__file__).parent

ref = (R/'test.asc').read_text()

ret = subprocess.check_output(exe, input=ref, universal_newlines=True, timeout=5)

match = 1
for line in ret.split('\n'):
    if '308 / 29' in line:
        match = 0
        break

raise SystemExit(match)
