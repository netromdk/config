#!/bin/sh
SELFPATH="$(
  cd -- "$(dirname "$0")" >/dev/null 2>&1
  pwd -P
)"
. ${SELFPATH}/util.lib.sh

check_program readlink

echo "+++ Deploying configs as symlinks +++"

find "${CONFIGS}" -type f | while IFS= read -r f; do
  relpath="${f#"${CONFIGS}"/}"
  deploy "$f" "${HOME}/$(dirname "${relpath}")"
done

echo "\n+++ Deploying scripts to PATH  +++"

mkdir -p "${PATHFLD}"
find "${SCRIPTS}" -type f | while IFS= read -r f; do
  deploy "$f" "${PATHFLD}"
done
