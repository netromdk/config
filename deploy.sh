#!/bin/sh
SELFPATH="$(cd -- "$(dirname "$0")" >/dev/null 2>&1; pwd -P)"
CONFIGS="${SELFPATH}/configs"
SCRIPTS="${SELFPATH}/scripts"
PATHFLD="${HOME}/.local/bin"

deploy() {
  SRC="$1"
  DSTFLD="$2"
  DST="${DSTFLD}/$(basename ${SRC})"
  echo "\n${DST} -> ${SRC}"
  if [ -L "${DST}" ]; then
    if [ "$(readlink ${DST})" = "${SRC}" ]; then
      echo "Already installed."
      return
    fi
  fi
  if [ -e "${DST}" ]; then
    echo "Destination exists. Ignoring."
    return
  fi
  ln -s "${SRC}" "${DST}" && echo "Installed."
}

echo "+++ Deploying configs and scripts as symlinks +++"

for f in $(find "${CONFIGS}" -type f) "${SCRIPTS}"; do
  deploy "$f" "${HOME}"
done

echo "\n+++ Deploying scripts to PATH  +++"

mkdir -p "${PATHFLD}"
for f in $(find "${SCRIPTS}" -type f); do
  deploy "$f" "${PATHFLD}"
done
