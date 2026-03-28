########################################
# General
########################################

HISTFILE=${HOME}/.histfile
HISTSIZE=100000
SAVEHIST=100000

# Ref: https://zsh.sourceforge.io/Doc/Release/Options.html#Options
setopt nobeep                 # Absolutely no beeping!
setopt notify                 # Report status of bg jobs immediately.
setopt correct                # Try to correct spelling of commands.
setopt extended_glob          # Treat the ‘#’, ‘~’ and ‘^’ chars as part of patterns for filename generation, etc.
setopt auto_cd                # Change directory without using paths.
setopt auto_pushd             # Make `cd` push the old directory onto the directory stack.
setopt pushd_to_home          # Have `pushd` with no arguments act like `pushd ${HOME}`.
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

function load-config {
  source "$HOME/.zshrc.d/$1"
}

load-config bindings.zsh
load-config exports.zsh
load-config prompt.zsh
load-config aliases.zsh
load-config functions.zsh
load-config fzf.zsh


##### Keep everything before this line! #####

# Load custom ZSH config if present.
if [ -e ${HOME}/.zshrc.custom ]; then
  . ${HOME}/.zshrc.custom
fi
