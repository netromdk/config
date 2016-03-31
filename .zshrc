HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
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
export PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:~/pkgconfig

alias l='ls -lh -G -F'
alias ls='ls -G -F'

alias h='history'
alias emacs='emacs -nw'
alias gdb='gdb -q'
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

### Antigen plugin manager ###
source ~/.antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle command-not-found
antigen bundle sudo # ESC-ESC puts sudo in front of last cmd.
antigen bundle colored-man-pages
antigen bundle last-working-dir # shells start at last working dir.

# Provides the 'k' command for better folder listing, including git status.
antigen bundle supercrabtree/k
alias k='k -h' # Force human-readable sizes.

# Provides 'extract' (and alias 'x') to extract a wide range of compressed files using their
# corresponding decompressor programs.
antigen bundle extract

# OS specific plugins
if [[ $CURRENT_OS == 'OS X' ]]; then
  # Gives the following commands:
  # tab           open the current directory in a new tab
  # pfd           return the path of the frontmost Finder window
  # pfs           return the current Finder selection
  # cdf           cd to the current Finder directory
  # pushdf        pushd to the current Finder directory
  # quick-look    quick Look a specified file
  # man-preview   open a specified man page in Preview
  # trash         move a specified file to the Trash
  antigen bundle osx
fi

antigen theme blinks

antigen apply
