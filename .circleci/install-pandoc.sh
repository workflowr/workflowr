#!/bin/bash
set -eux

# https://pandoc.org/installing.html#linux
# https://github.com/jgm/pandoc/releases

ver=$1

url="https://github.com/jgm/pandoc/releases/download/${ver}/pandoc-${ver}-1-amd64.deb"
wget -P /tmp $url
dpkg -i /tmp/pandoc-${ver}-1-amd64.deb
pandoc -v
