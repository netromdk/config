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

  echo "Installing missing packages updates from APT, Snap, and Flatpak."
  echo "\n======= APT ======="
  check_program apt
  if [ $? -eq 0 ]; then
    set -x
    sudo apt update
    sudo apt install \
      zsh \
      ninja-build \
      build-essential \
      make \
      curl \
      wget \
      gpg \
      git \
      git-lfs \
      gh \
      ccache \
      htop \
      ripgrep \
      silversearcher-ag \
      cppcheck \
      shellcheck \
      aspell \
      aspell-en \
      aspell-da \
      python3.14-full \
      python3.14-venv \
      python3-pip \
      python-is-python3 \
      rustup

    # Add Kitware apt source for cmake.
    if [ ! -e "/etc/apt/sources.list.d/kitware.list" ]; then
      sudo wget -qO - https://apt.kitware.com/keys/kitware-archive-latest.asc \
        | gpg --dearmor | sudo tee /usr/share/keyrings/kitware-archive-keyring.gpg > /dev/null
      sudo sh -c "echo 'deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] \
https://apt.kitware.com/ubuntu/ ${CODENAME} main' > /etc/apt/sources.list.d/kitware.list"
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
  pip install --break-system-packages \
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
