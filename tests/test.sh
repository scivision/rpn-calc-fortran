#!/bin/bash

rpncalc < test.asc | tail -n1 | grep 10.600
