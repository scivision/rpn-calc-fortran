#!/usr/bin/env python
import subprocess
import sys
import shutil

exe = shutil.which(sys.argv[1])
if not exe:
    print('executable', sys.argv[1], 'not found', file=sys.stderr)
    raise SystemExit(77)

ret = subprocess.run(exe, input="2 5 *", universal_newlines=True, timeout=5)
raise SystemExit(ret.returncode)
