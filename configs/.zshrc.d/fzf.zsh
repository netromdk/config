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

# C-x-r directly executes a historic command. Instead of pressing enter after again.
fzf-history-widget-accept() {
  fzf-history-widget
  zle accept-line
}
zle     -N     fzf-history-widget-accept
bindkey '^X^R' fzf-history-widget-accept

# Set up fzf key bindings and fuzzy completion.
source <(fzf --zsh)
