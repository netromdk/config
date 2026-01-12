if [ "${SELFPATH}" = "" ]; then
  echo "'\$SELFPATH' must be set before including 'util.lib.sh'!"
  exit 1
fi

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
