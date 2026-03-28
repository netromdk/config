# Use to build target(s) by autodetecting from "pwd" and using build program $1 to build sub
# build-folder $2. Requires the build folders to reside in "build".
function _build {
  BUILDER=$1
  shift

  SUBFOLDER=$1
  shift

  TRYDIR=$(pwd)

  BUILDFOLDER=""
  for try in 1 2 3 4 5 6 7 8 9 10; do
    BUILDFOLDER="${TRYDIR}/build"
    if [ -d "${BUILDFOLDER}" ]; then
      break
    fi
    TRYDIR="${TRYDIR}/.."
  done

  if [ -z $BUILDFOLDER ]; then
    echo "Didn't find any build/ folder!"
    return
  fi

  if [ ! ${BUILDER} = "ctest" ]; then
    ${BUILDER} -C "${BUILDFOLDER}/${SUBFOLDER}" $@
  else
    cd "${BUILDFOLDER}/${SUBFOLDER}" && ${BUILDER} $@
  fi
}

function ninjabuild {
  _build ninja $@
}

function makebuild {
  _build make $@
}

function ctestbuild {
  _build ctest $@
}
