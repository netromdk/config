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
# C-y to copy into clipboard with `wl-copy`.
FZF_CTRL_R_OPTS="
  --no-sort
  --preview 'echo {}'
  --preview-window down:3:hidden:wrap
  --bind '?:toggle-preview'
  --bind 'ctrl-y:execute-silent(echo -n {2..} | wl-copy)+abort'
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

# Advanced completion (**) customization of fzf options via _fzf_comprun function.
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'tree -C -a {} | head -200'   "$@" ;;
    export|unset) fzf --preview "eval 'echo \$'{}"            "$@" ;;
    ssh)          fzf --preview 'dig {}'                      "$@" ;;
    *)            fzf --preview 'bat -n --color=always {}'    "$@" ;;
  esac
}

# C-x-r directly executes a historic command. Instead of pressing enter after again.
fzf-history-widget-accept() {
  fzf-history-widget
  zle accept-line
}
zle     -N     fzf-history-widget-accept
bindkey '^X^R' fzf-history-widget-accept

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

# Set up fzf key bindings and fuzzy completion.
source <(fzf --zsh)
