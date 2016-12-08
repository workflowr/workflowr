#!/bin/bash

# The following commands capture information about the command line
# arguments:
#
# $1, $2, etc. - the first, second, etc., argument
# $* - all the arguments
# $# - the total number of arguments
#
# Usage:
#
# ./script.sh ex1 ex2 ex3
#

i=1
for arg in $*
do
  echo "Argument $i: $arg"
  let i=$i+1
done
