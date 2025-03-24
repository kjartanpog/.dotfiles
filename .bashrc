set -o vi
export EDITOR="emacs -nw"
alias e="emacs -nw"
eval "$(direnv hook bash)"
source "$(dirname "$(dirname "$(readlink "$(whereis git | awk '{print $2}')")")")/share/git/contrib/completion/git-prompt.sh"
PROMPT_COMMAND='PS1_CMD1=$(__git_ps1 " (%s)")'; PS1='\n\[\e]0;\u@\h: \w\a\]\[\e[32m\]\u\[\e[97m\]@\h\[\e[0m\] \[\e[32;1m\]\w\[\e[0m\]${PS1_CMD1}> '

# if [ -f ~/.common_aliases ]; then
#     source ~/.common_aliases
# fi
