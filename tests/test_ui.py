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

ret = subprocess.run(str(exe), input="2 5 *", universal_newlines=True, timeout=5)
raise SystemExit(ret.returncode)
