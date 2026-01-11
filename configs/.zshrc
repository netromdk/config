HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

setopt autocd notify correct hist_ignore_dups no_hist_beep hist_reduce_blanks interactive_comments pushd_to_home auto_list extendedglob prompt_subst nobeep appendhistory
unsetopt nomatch beep

zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit

bindkey -e
bindkey "\eOH" beginning-of-line # Home-key
bindkey "\eOF" end-of-line # End-key
bindkey "^[[1;6C" end-of-line
bindkey "^[[1;6D" beginning-of-line
bindkey "[[6C" end-of-line
bindkey "[[6D" beginning-of-line
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line

export NOBEEP=YES
export CLICOLOR=YES
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export TZ="Europe/Copenhagen"
export EDITOR=emacs

if [ -e $HOME/.local/bin ]; then
  export PATH=$HOME/.local/bin:$PATH
fi

# If flatpak is installed then setup folders for XDG.
if [ -e /var/lib/flatpak/exports/share ]; then
  export XDG_DATA_DIRS=/var/lib/flatpak/exports/share:$XDG_DATA_DIRS
fi
if [ -e $HOME/.local/share/flatpak/exports/share ]; then
  export XDG_DATA_DIRS=$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS
fi

# Enable color support of ls and grep variants.
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias h='history'
alias tunnel='ssh -N -f -q'

alias ...='cd ..; cd ..'
alias ....='cd ..; cd ..; cd ..'
alias .....='cd ..; cd ..; cd ..; cd ..'

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

##### Keep everything before this line! #####

# Load custom ZSH config if present.
if [ -e $HOME/.zshrc.custom ]; then
  . $HOME/.zshrc.custom
fi
