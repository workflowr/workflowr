#!/bin/bash
set -eux

apt-get update
apt-get install -y \
  emacs \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  nano \
  qpdf \
  wget \
  zlib1g-dev
