shopt -s expand_aliases

force_color_prompt=yes
alias ls='ls --color=auto'
alias py='python3'
alias python='python3'
alias lsa='ls -a'
alias vi='nvim'
alias lsl='ls -l'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias sshuio='ssh -J matande@login.uio.no matande@login.ifi.uio.no'

export PS1='\w/ '

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"  # ghcup-env
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"

# THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"
