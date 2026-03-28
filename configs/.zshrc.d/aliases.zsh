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

# An improved `cat`.
if hash batcat 2> /dev/null; then
  # An alias does not work with FZF in preview!
  if [ ! -L "$HOME/.local/bin/bat" ]; then
    ln -s "$(which batcat)" "$HOME/.local/bin/bat" >/dev/null
  fi
fi
