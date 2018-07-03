#!/bin/bash

rpncalc < test.asc | tail -n1 | grep "308 / 29"
