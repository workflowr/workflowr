#!/bin/bash
set -eu

# Setup r2u to install CRAN packages as Ubuntu binaries via APT
#
# https://github.com/eddelbuettel/r2u
# https://github.com/eddelbuettel/r2u/blob/master/inst/scripts/add_cranapt_jammy.sh

apt-get update
apt-get install --yes --no-install-recommends wget ca-certificates
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
  | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
echo "deb [arch=amd64] https://dirk.eddelbuettel.com/cranapt jammy main" \
  > /etc/apt/sources.list.d/cranapt.list
apt-get update
wget -q -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
  | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" \
  > /etc/apt/sources.list.d/cran_r.list
echo "Package: *" > /etc/apt/preferences.d/99cranapt
echo "Pin: release o=CRAN-Apt Project" >> /etc/apt/preferences.d/99cranapt
echo "Pin: release l=CRAN-Apt Packages" >> /etc/apt/preferences.d/99cranapt
echo "Pin-Priority: 700"  >> /etc/apt/preferences.d/99cranapt
