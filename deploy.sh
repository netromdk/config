#!/bin/sh
SELFPATH="$(cd -- "$(dirname "$0")" >/dev/null 2>&1; pwd -P)"
. ${SELFPATH}/util.lib.sh

check_program readlink

echo "+++ Deploying configs as symlinks +++"

for f in $(find "${CONFIGS}" -maxdepth 1); do
  if [ ! "$f" = "${CONFIGS}" ]; then
    deploy "$f" "${HOME}"
  fi
done

echo "\n+++ Deploying scripts to PATH  +++"

mkdir -p "${PATHFLD}"
for f in $(find "${SCRIPTS}" -type f); do
  deploy "$f" "${PATHFLD}"
done
