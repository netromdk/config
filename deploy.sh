#!/bin/sh
SELFPATH="$(cd -- "$(dirname "$0")" >/dev/null 2>&1; pwd -P)"
CONFIGS="${SELFPATH}/configs"
SCRIPTS="${SELFPATH}/scripts"

echo "+++ Deploying configs and scripts as symlinks +++"

for f in $(find "${CONFIGS}" -type f) "${SCRIPTS}"; do
  dst="${HOME}/$(basename $f)"
  echo "\n${dst} -> $f"
  if [ -L "${dst}" ]; then
    link="$(readlink ${dst})"
    if [ "${link}" = "$f" ]; then
      echo "Already installed."
      continue
    fi
  fi
  if [ -e "${dst}" ]; then
    echo "Destination exists. Ignoring."
    continue
  fi
  ln -s "$f" "${dst}" && echo "Installed."
done
