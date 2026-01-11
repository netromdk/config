#!/bin/sh
echo "Checking for package updates via APT, Snap, and Flatpak."

check_program() {
  if hash $1 2> /dev/null; then
    return 0
  fi
  echo "'$1' not installed"
  return 1
}

echo "\n======= APT ======="
check_program apt
if [ $? -eq 0 ]; then
  sudo apt update
  apt list --upgradable
  sudo apt upgrade
fi

echo "\n======= Snap ======="
check_program snap
if [ $? -eq 0 ]; then
  sudo snap refresh
fi

echo "\n======= Flatpak ======="
check_program flatpak
if [ $? -eq 0 ]; then
  flatpak update
fi
