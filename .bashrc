export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\][\u@\h\[\033[01;34m\] \W]\$\[\033[00m\] '

alias emacsclient="emacsclient -nc"
export EDITOR=vim
alias l="less"

export HISTTIMEFORMAT='%F %T '

export NVM_DIR="/Users/daniel/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
