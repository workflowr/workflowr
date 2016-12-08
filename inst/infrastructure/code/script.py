#!/usr/bin/env python

# Command line arguments can be read with sys.argv. The first element
# is skipped because it is the name of the file.

# Usage:
#
# ./script.py ex1 ex2 ex3
#

import sys

args = sys.argv[1:]

for i in range(len(args)):
    sys.stdout.write("Argument %d: %s\n"%(i + 1, args[i]))
