#! /bin/bash

sudo pacman -Syu --needed \
  libsodium \
  qpdf
./install_me.R
xdg-open docs/index.html
