export NOBEEP=YES
export CLICOLOR=YES
export TZ="Europe/Copenhagen"
export EDITOR=emacs

# Language and Unicode.
LANG=en_US.UTF-8
LANGUAGE=en_US:en
LC_ALL=en_US.UTF-8
LC_ADDRESS=en_US.UTF-8
LC_NAME=en_US.UTF-8
LC_MONETARY=en_US.UTF-8
LC_PAPER=en_US.UTF-8
LC_IDENTIFICATION=en_US.UTF-8
LC_TELEPHONE=en_US.UTF-8
LC_MEASUREMENT=en_US.UTF-8
LC_TIME=en_US.UTF-8
LC_NUMERIC=en_US.UTF-8

# If flatpak is installed then setup folders for XDG.
if [ -e /var/lib/flatpak/exports/share ]; then
  export XDG_DATA_DIRS=/var/lib/flatpak/exports/share:${XDG_DATA_DIRS}
fi
if [ -e ${HOME}/.local/share/flatpak/exports/share ]; then
  export XDG_DATA_DIRS=${HOME}/.local/share/flatpak/exports/share:${XDG_DATA_DIRS}
fi
