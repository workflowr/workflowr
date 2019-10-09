#!/bin/bash
set -eux

echo "Building package"
R CMD build --no-manual .

echo "Checking package"
R CMD check --as-cran --no-examples --no-manual *tar.gz
