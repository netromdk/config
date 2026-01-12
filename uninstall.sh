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

echo "+++ Placing copies instead of symlinks to configs and scripts +++"

for f in $(find "${CONFIGS}" -type f) "${SCRIPTS}"; do
  DST="${HOME}/$(basename $f)"
  echo "\n${TAB}${DST} -> $f"
  if [ -L "${DST}" ]; then
    if [ "$(readlink ${DST})" = "$f" ]; then
      rm -rf "${DST}"
      cp -a "$f" "${DST}"
      echo "${TAB}${TAB}Placing copy."
      continue
    fi
  fi
  echo "${TAB}${TAB}Nothing to do."
done

echo "\n+++ Removing scripts from PATH  +++"

for f in $(find "${SCRIPTS}" -type f); do
  DST="${PATHFLD}/$(basename $f)"
  echo "\n${TAB}${DST} -> $f"
  if [ -e "${DST}" ]; then
    rm -f "${DST}"
    echo "${TAB}${TAB}Removing."
    continue
  fi
  echo "${TAB}${TAB}Nothing to do."
done
