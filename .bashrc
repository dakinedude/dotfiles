shopt -s expand_aliases
force_color_prompt=yes

# Aliases
alias ls='ls --color=auto'
alias py='python3'
alias python='python3'
alias lsa='ls -a'
alias lsl='ls -l'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias vi='nvim'
alias sshuio='ssh -J matande@login.uio.no matande@login.ifi.uio.no'
alias em='emacsclient -c -a ""'
alias emt='emacs -nw'

# Prompt
export PS1='\w/ '

# Environments
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export PATH="$HOME/bin"
PATH="$PATH:$HOME/.sdkman/candidates/java/current/bin"
PATH="$PATH:$HOME/.cargo/bin"
PATH="$PATH:$HOME/.cabal/bin"
PATH="$PATH:$HOME/.ghcup/bin"
PATH="$PATH:/usr/local/go/bin"
PATH="$PATH:$HOME/go/bin"
PATH="$PATH:/usr/local/bin"
PATH="$PATH:/usr/bin"
PATH="$PATH:/bin"

# SDKMAN (must be last)
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"
