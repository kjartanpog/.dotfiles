set -o vi
export EDITOR="emacs -nw"
alias e="emacs -nw"
alias gs="git status -s"
alias rbs="sudo nixos-rebuild switch -v --flake /home/kjartanm/Nix#Z13"
alias rbb="sudo nixos-rebuild boot -v --flake /home/kjartanm/Nix#Z13"
alias doomshell="nix develop ~/src/doomemacs-flake/"
eval "$(direnv hook bash)"
if command -v starship &> /dev/null; then
    eval "$(starship init bash)"
fi
# source "$(dirname "$(dirname "$(readlink "$(whereis git | awk '{print $2}')")")")/share/git/contrib/completion/git-prompt.sh"
# PROMPT_COMMAND='PS1_CMD1=$(__git_ps1 " (%s)")'; PS1='\n\[\e]0;\u@\h: \w\a\]\[\e[32m\]\u\[\e[97m\]@\h\[\e[0m\] \[\e[32;1m\]\w\[\e[0m\]${PS1_CMD1}> '

# if [ -f ~/.common_aliases ]; then
#     source ~/.common_aliases
# fi
