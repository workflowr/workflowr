#!/bin/bash
set -eu

# Utility script for mass replacement of text in R scripts:
#
# Usage: bash scripts/sed.sh original replacement
#
# Example: bash scripts/sed.sh git2r_head git2r::repository_head
#
# For regular expressions, easier to just manually run code similar to below.
# Very difficult to pass regex's via arguments and pass them correctly
# formatted for sed.

if [ ! -f DESCRIPTION ]
then
  echo "sed.sh must be executed in the root directory of the package"
  exit 1
fi

original=$1
replacement=$2

echo "Updating R scripts with s/${original}/${replacement}/g"

for f in R/*R tests/testthat/*R
do
  echo "Editing file $f"
  #grep 'file\.create' $f
  sed -i -e s/'${original}'/'${replacement}'/g $f
done
