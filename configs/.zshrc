########################################
# General
########################################

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

# Ref: https://zsh.sourceforge.io/Doc/Release/Options.html#Options
setopt nobeep                 # Absolutely no beeping!
setopt notify                 # Report status of bg jobs immediately.
setopt correct                # Try to correct spelling of commands.
setopt extended_glob          # Treat the ‘#’, ‘~’ and ‘^’ chars as part of patterns for filename generation, etc.
setopt auto_cd                # Change directory without using paths.
setopt auto_pushd             # Make `cd` push the old directory onto the directory stack.
setopt pushd_to_home          # Have `pushd` with no arguments act like `pushd $HOME`.
setopt auto_list              # Automatically list choices on an ambiguous completion.
setopt inc_append_history     # Adds commands as they are typed, not at shell exit.
setopt extended_history       # Save more info to history file: <beginning time>:<elapsed seconds>;<command>
setopt hist_expire_dups_first # Expire duplicates first.
setopt hist_ignore_dups       # Do not store duplications.
setopt hist_find_no_dups      # Ignore duplicates when searching.
setopt hist_reduce_blanks     # Removes blank lines from history.
setopt interactive_comments   # Allow comments even in interactive shells.

unsetopt nomatch

zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit


########################################
# Key bindings
########################################

bindkey -e # Emacs
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


########################################
# Variable exports and PATH
########################################

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

# Prefer binaries in local bin folders.
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


########################################
# Prompt
########################################

# Expansions: https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html#Prompt-Expansion
# xterm-256color cheat sheet: https://www.ditig.com/256-colors-cheat-sheet
# VCS info: https://zsh.sourceforge.io/Doc/Release/User-Contributions.html#Version-Control-Information

PROMPT="%(?.%F{10}⏺.%F{9}%B?%?%b)%f [%n] %B%~%b %F{10}%#%f "

autoload -Uz add-zsh-hook vcs_info
setopt prompt_subst
add-zsh-hook precmd vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats '%b%u%c'
# Format when the repo is in an action (merge, rebase, etc)
zstyle ':vcs_info:git*' actionformats '%F{14}⏱ %*%f'
zstyle ':vcs_info:git*' unstagedstr '*'
zstyle ':vcs_info:git*' stagedstr '+'
# This enables %u and %c (unstaged/staged changes) to work,
# but can be slow on large repos
zstyle ':vcs_info:*:*' check-for-changes true

RPROMPT="%F{240}\$vcs_info_msg_0_%f"


########################################
# Aliases
########################################

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

# Commands I write wrong sometimes.
alias nija='ninja'
alias inja='ninja'
alias ninj='ninja'


########################################
# Functions
########################################

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
