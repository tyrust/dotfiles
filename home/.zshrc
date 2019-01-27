# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="../../.zsh/blinks-tyrus"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ssh-agent virtualenv)

source $ZSH/oh-my-zsh.sh

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Editors
export EDITOR='emacsclient'
export ALTERNATE_EDITOR=""
e() {
  nohup emacsclient $@ > /dev/null 2>&1 &
}

if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi

function alert() { echo -n \\a }
precmd_functions+=(alert)

# Add colors to `man`.
man() {
    env \
        LESS_TERMCAP_mb=$'\e[1;32m' \
        LESS_TERMCAP_md=$'\e[1;38;5;136m' \
        LESS_TERMCAP_me=$'\e[0m' \
        LESS_TERMCAP_se=$'\e[0m' \
        LESS_TERMCAP_so=$'\e[38;5;246m' \
        LESS_TERMCAP_ue=$'\e[0m' \
        LESS_TERMCAP_us=$'\e[04;38;5;146m' \
            man "$@"
}

function tm() {
    [[ -z "$1" ]] && { echo "usage: tm <session>" >&2; return 1; }
    tmux new -ADs $1
}

function __tmux-sessions() {
    local -a sessions
    sessions=(${(f)"$(tmux list-sessions -F '#{session_name}')"})
    _describe -t sessions 'sessions' sessions
}
compdef __tmux-sessions tm

if [ -f ~/.bash_aliases ]; then
  source ~/.bash_aliases
fi

if [ -f ~/.bash_extras ]; then
  source ~/.bash_extras
fi
