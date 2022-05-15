#!/usr/bin/env python3

from __future__ import annotations
from pathlib import Path
import argparse
import subprocess
import scipy.special as sp
import pytest
from pytest import approx


p = argparse.ArgumentParser(description="stdin test fixture")
p.add_argument("exe", help="rpncalc executable")
P = p.parse_args()

EXE = P.exe
assert Path(EXE).is_file(), "rpncalc executable not found"


def frun(name: str, args) -> float:
    """Runs a command and returns its output"""
    if not isinstance(args, (list, tuple)):
        args = [args]
    input = "\n".join(map(str, args)) + "\n" + name
    raw = subprocess.run([EXE], capture_output=True, input=input, text=True, timeout=5)
    assert not raw.stderr, raw.stderr
    return float(raw.stdout.strip().split("\n")[-1])


def test_bessel0():

    fcns = {"besselj0": sp.jv, "bessely0": sp.yn, "besseli0": sp.iv, "besselk0": sp.kn}
    args = [3, 1]

    for k, f in fcns.items():
        for a in args:
            assert frun(k, a) == approx(f(0, a))


def test_bessel1():

    fcns = {"besselj1": sp.jv, "bessely1": sp.yn, "besseli1": sp.iv, "besselk1": sp.kn}
    args = [3, 1]

    for k, f in fcns.items():
        for a in args:
            assert frun(k, a) == approx(f(1, a))


def test_bessel_general():

    fcns = {"besselj": sp.jv, "bessely": sp.yv, "besseli": sp.iv, "besselk": sp.kv}
    args = [(2, 3), (5, 1)]

    for k, f in fcns.items():
        for a in args:
            assert frun(k, a) == approx(f(*a))


def test_riemann_zeta():

    assert frun("rzeta", (2)) == approx(sp.zeta(2))


if __name__ == "__main__":
    pytest.main([__file__])
