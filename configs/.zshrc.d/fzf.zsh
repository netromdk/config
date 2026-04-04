FZF_DEFAULT_OPTS="
  --style full
  --input-label ' Input '
  --bind 'result:transform-list-label:
    if [[ -z $FZF_QUERY ]]; then
      echo \" $FZF_MATCH_COUNT items \"
    else
      echo \" $FZF_MATCH_COUNT matches for [$FZF_QUERY] \"
    fi
    '
  --bind 'focus:transform-preview-label:[[ -f {} ]] && printf \" Previewing [%s] \" {}'
  --bind 'ctrl-r:change-list-label( Reloading the list )+reload(sleep 2; git ls-files)'
  --bind 'ctrl-/:change-preview-window(hidden|50%)'
  --color 'border:#3c414c,label:#e2e2e5'"

# Preview file content using bat (https://github.com/sharkdp/bat).
# `--select-1` automatically selects the item if there's only one.
# `--exit-0` automatically exits when the list is empty.
FZF_CTRL_T_OPTS="
  --walker-skip .git,node_modules,target
  --preview '[[ -f {} ]] && bat -n --color=always {} || echo \"No file to preview.\"'
  --bind 'focus:+transform-header:[[ -f {} ]] && file --brief {} || echo \"No file selected.\"'
  --header-label ' File Type '
  --select-1
  --exit-0"

# `--no-sort` show chronological order of historic commands, no sorting.
# Preview commands that are too long to show in the list by hitting ?.
# C-y to copy into clipboard.
FZF_CTRL_R_OPTS="
  --no-sort
  --preview 'echo {}'
  --preview-window down:3:hidden:wrap
  --bind '?:toggle-preview'
  --bind 'ctrl-y:execute-silent(echo -n {2..} | ${COPY_CMD})+abort'
  --color header:italic
  --header 'C-y to copy command into clipboard.'"

# Print tree structure in the preview window.
FZF_ALT_C_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'tree -C -a {}'
  --select-1
  --exit-0"

# Options for path completion (e.g. nano **<TAB>)
FZF_COMPLETION_PATH_OPTS="
  --walker file,dir,follow,hidden
  ${FZF_CTRL_T_OPTS}"

# Options for directory completion (e.g. cd **<TAB>)
FZF_COMPLETION_DIR_OPTS="
  --walker dir,follow,hidden
  ${FZF_ALT_C_OPTS}"

# C-x-r directly executes a historic command. Instead of pressing enter after again.
fzf-history-widget-accept() {
  fzf-history-widget
  zle accept-line
}
zle     -N     fzf-history-widget-accept
bindkey '^X^R' fzf-history-widget-accept

# C-h shows man pages with preview using `bat`, where enter shows fullscreen inside `bat` and
# returns to the list afterwards.
fzf-man-widget() {
  manpage="echo {} | sed 's/\([[:alnum:][:punct:]]*\) (\([[:alnum:]]*\)).*/\2 \1/'"
  batman="${manpage} | xargs -r man | col -bx | bat -l man -p --color always"
  man -k . | \
    sort | \
    awk -v cyan=$(tput setaf 6) -v blue=$(tput setaf 4) -v res=$(tput sgr0) -v bld=$(tput bold) \
      '{ $1=cyan bld $1; $2=res blue $2; } 1' | \
    fzf -q "$1" \
      --ansi \
      --tiebreak=begin \
      --prompt='man > '  \
      --preview-window '50%,rounded,<50(up,85%,border-bottom)' \
      --preview "${batman}" \
      --bind "enter:execute(${batman})"
  zle reset-prompt
}
zle -N fzf-man-widget
bindkey '^h' fzf-man-widget

fzf-apt-install-widget() {
  aptitude search -F '%p' --disable-columns '!~i' | grep -v "\\$" | \
    sort | \
    fzf -q "$1" \
        --prompt="APT Install > " \
        --height=40 \
        --preview-window="right:60%" \
        --preview-label=" Package Info " \
        --preview="apt-cache show {} | bat -l yaml -p --color=always" \
        --bind "enter:execute(sudo apt install {})"
  zle reset-prompt
}
zle -N fzf-apt-install-widget
bindkey '^[a^[i' fzf-apt-install-widget

fzf-apt-uninstall-widget() {
  FZF_APT_PURGE="/tmp/fzf-apt-purge"
  aptitude search -F '%p' --disable-columns '~i' | \
    sort | \
    fzf -q "$1" \
        --prompt="APT Remove > " \
        --height=40 \
        --preview-window="right:60%" \
        --preview-label=" Package Info " \
        --preview="apt-cache show {} | bat -l yaml -p --color=always" \
        --header '[ M-p: Purge | M-r: Remove ]' \
        --bind "enter:execute(if [[ -f ${FZF_APT_PURGE} ]]; then sudo apt purge {}; else sudo apt remove {}; fi )" \
        --bind "alt-r:+execute-silent(rm -f ${FZF_APT_PURGE} >/dev/null)+change-prompt(APT Remove > )" \
        --bind "alt-p:+execute-silent(touch ${FZF_APT_PURGE})+change-prompt(APT Purge > )"
  rm -f ${FZF_APT_PURGE} >/dev/null
  zle reset-prompt
}
zle -N fzf-apt-uninstall-widget
bindkey '^[a^[u' fzf-apt-uninstall-widget

# List APT packages with preview. Mostly for advanced completion (`_fzf_comprun`).
fzf-apt-pkg-comp() {
  aptitude search -F '%p' --disable-columns '!~i' | grep -v "\\$" | \
    sort | \
    fzf --layout=reverse \
        --info=inline \
        --height=40 \
        --preview-window="right:60%" \
        --preview-label=" Package Info " \
        --preview="apt-cache show {} | bat -l yaml -p --color=always"
}

# find-in-file - usage: fif <search term>
# Using ripgrep combined with bat preview.
fif() {
  if [ ! "$#" -gt 0 ]; then
    echo "Need a string to search for!";
    return 1;
  fi
  rg --files-with-matches --no-messages "$1" | \
    fzf --preview "bat --color=always --style=plain --paging=never {} \
                   | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 5 '$1' \
                     || rg --ignore-case --pretty --context 5 '$1' {}"
}

# fkill - list only the current user can kill.
fkill() {
  local pid
  if [ "$UID" != "0" ]; then
    pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
  else
    pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
  fi

  if [ "x$pid" != "x" ]; then
    echo $pid | xargs kill -${1:-9}
  fi
}

# Advanced completion (**) customization of fzf options via _fzf_comprun function.
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'tree -C -a {} | head -200'   "$@" ;;
    export|unset) fzf --preview "eval 'echo \$'{}"            "$@" ;;
    ssh)          fzf --preview 'dig {}'                      "$@" ;;
    apt)          fzf-apt-pkg-comp                            "$@" ;;
    *)            fzf --preview 'bat -n --color=always {}'    "$@" ;;
  esac
}

# Set up fzf key bindings and fuzzy completion.
source <(fzf --zsh)
