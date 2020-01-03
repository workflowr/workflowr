#!/bin/bash
set -eu

# Convenience script for running `R CMD check`. Must be run from the root of the
# project directory.
#
# Optional arguments are passed directly to `R CMD check`.

if [ ! -f DESCRIPTION ]
then
  echo "build.sh must be executed in the root directory of the package"
  exit 1
fi

version=`cat DESCRIPTION | grep Version | cut -d" " -f2`
echo Package version: $version

source="workflowr_${version}.tar.gz"
rcheck="workflowr.Rcheck"

if [ -f "$source" ]
then
  rm "$source"
fi

if [ -d "$rcheck" ]
then
  rm -r "$rcheck"
fi

echo "Buiding..."
R CMD build --no-manual .

echo "Checking..."
echo "$*"
R CMD check --as-cran --no-manual $* "$source"
