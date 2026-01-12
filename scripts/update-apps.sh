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
  set -x
  sudo apt update
  apt list --upgradable
  sudo apt upgrade
  set +x
fi

echo "\n======= Snap ======="
check_program snap
if [ $? -eq 0 ]; then
  CMD="sudo snap refresh --color=auto"
  set -x
  # "All snaps up to date." is written to stderr and we want it ignored and only written with the
  # last snap invocation.
  ${CMD} --list 2>/dev/null
  set +x
  NUMPKGS=$(${CMD} --list 2>/dev/null | wc -l)
  if [ $NUMPKGS -gt 0 ]; then
    echo "Note: Update running apps by rerunning $0 after terminating them."
  fi
  set -x
  ${CMD}
  set +x
fi

echo "\n======= Flatpak ======="
check_program flatpak
if [ $? -eq 0 ]; then
  set -x
  flatpak update
fi
