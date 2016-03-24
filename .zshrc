HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
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

function precmd {
 export RPS1="`date '+%H:%M:%S'`"
}

prompt="$(print '%{\e[0m%}%B%n@%m%b%{\e[0m%}') %~ %#$(print '%{\e[1;31m%}>%{\e[0m%}') "

alias ls='ls -G -F'
alias x='startx'
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
