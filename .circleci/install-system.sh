#!/bin/bash
set -eux

apt-get update
apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  qpdf \
  wget \
  zlib1g-dev
