#!/bin/sh
SELFPATH="$(cd -- "$(dirname "$0")" >/dev/null 2>&1; pwd -P)"
CONFIGS="${SELFPATH}/configs"
SCRIPTS="${SELFPATH}/scripts"
PATHFLD="${HOME}/.local/bin"
TAB="  "

check_program() {
  if ! hash $1 2> /dev/null; then
    echo "'$1' is not installed!"
    exit 1
  fi
}
check_program readlink

deploy() {
  SRC="$1"
  DSTFLD="$2"
  DST="${DSTFLD}/$(basename ${SRC})"
  echo "\n${TAB}${DST} -> ${SRC}"
  if [ -L "${DST}" ]; then
    if [ "$(readlink ${DST})" = "${SRC}" ]; then
      echo "${TAB}${TAB}Already installed."
      return
    fi
  fi
  if [ -e "${DST}" ]; then
    echo "${TAB}${TAB}Destination exists. Ignoring."
    return
  fi
  ln -s "${SRC}" "${DST}" && echo "Installed."
}

echo "+++ Deploying configs as symlinks +++"

for f in $(find "${CONFIGS}" -type f); do
  deploy "$f" "${HOME}"
done

echo "\n+++ Deploying scripts to PATH  +++"

mkdir -p "${PATHFLD}"
for f in $(find "${SCRIPTS}" -type f); do
  deploy "$f" "${PATHFLD}"
done
