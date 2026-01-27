#!/bin/sh

check_program() {
  if hash $1 2> /dev/null; then
    return 0
  fi
  echo "'$1' not installed"
  return 1
}

DIST=$(uname -s)

if [ "${DIST}" = "Linux" ]; then
  CODENAME=$(lsb_release -c -s | tail -n 1)
  APT_LIST_D="/etc/apt/sources.list.d/"
  KEYRINGS="/usr/share/keyrings/"
  KITWARE_APT_LIST="${APT_LIST_D}/kitware.list"
  KITWARE_APT_URL="https://apt.kitware.com"

  echo "Installing missing packages updates from APT, Snap, and Flatpak."
  echo "\n======= APT ======="
  check_program apt
  if [ $? -eq 0 ]; then
    set -x
    sudo apt update
    sudo apt install \
      aspell \
      aspell-da \
      aspell-en \
      build-essential \
      ccache \
      cppcheck \
      curl \
      gh \
      git \
      git-lfs \
      gpg \
      htop \
      make \
      ninja-build \
      python-is-python3 \
      python3-pip \
      python3.14-full \
      python3.14-venv \
      ripgrep \
      rustup \
      shellcheck \
      silversearcher-ag \
      tree \
      wget \
      zsh

    # Add Kitware APT source for cmake.
    if [ ! -e "${KITWARE_APT_LIST}" ]; then
      sudo wget -qO - ${KITWARE_APT_URL}/keys/kitware-archive-latest.asc \
        | gpg --dearmor | sudo tee ${KEYRINGS}/kitware-archive-keyring.gpg > /dev/null
      sudo sh -c "echo 'deb [signed-by=${KEYRINGS}/kitware-archive-keyring.gpg] \
${KITWARE_APT_URL}/ubuntu/ ${CODENAME} main' > ${KITWARE_APT_LIST}"
      sudo apt update
    fi
    sudo apt install cmake

    set +x
  fi

  echo "\n======= Snap ======="
  check_program snap
  if [ $? -eq 0 ]; then
    set -x
    sudo snap refresh
    sudo snap install emacs
    set +x
  fi

  echo "\n======= Flatpak ======="
  check_program flatpak
  if [ $? -eq 0 ]; then
    set -x
    flatpak update
    flatpak install \
      org.signal.Signal \
      net.waterfox.waterfox \
      me.proton.Mail \
      me.proton.Pass
    set +x
  fi

elif [ "${DIST}" = "Darwin" ]; then
  echo "Installing missing packages updates from Homebrew."
  check_program brew
  if [ $? -eq 0 ]; then
    set -x
    brew update
    brew install \
      curl \
      git \
      ripgrep \
      the_silver_searcher \
      cppcheck \
      shellcheck \
      aspell
    set +x
  fi
fi

echo "\n======= Pip ======="
check_program pip
if [ $? -eq 0 ]; then
  echo "Installing missing packages updates from Pip."
  set -x
  pip install --user --break-system-packages \
    python-lsp-server \
    flake8 \
    bandit
  set +x
fi

echo "\n======= Rust ======="
check_program rustup
if [ $? -eq 0 ]; then
  echo "Installing missing packages updates from Rust."
  set -x
  rustup update
  rustup component add rustfmt rust-analysis rust-src
  set +x
fi
