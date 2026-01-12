#!/bin/sh
SELFPATH="$(cd -- "$(dirname "$0")" >/dev/null 2>&1; pwd -P)"
. ${SELFPATH}/util.lib.sh

check_program readlink

echo "+++ Deploying configs as symlinks +++"

for f in $(find "${CONFIGS}" -type f); do
  deploy "$f" "${HOME}"
done

echo "\n+++ Deploying scripts to PATH  +++"

mkdir -p "${PATHFLD}"
for f in $(find "${SCRIPTS}" -type f); do
  deploy "$f" "${PATHFLD}"
done
