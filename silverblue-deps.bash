#! /bin/bash

echo "Installing packages"
sudo rpm-ostree install --allow-inactive \
  libsodium-devel
echo "Now reboot with 'sudo systemctl reboot'"
