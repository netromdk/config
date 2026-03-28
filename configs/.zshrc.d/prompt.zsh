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
