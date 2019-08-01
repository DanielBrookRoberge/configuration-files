source ~/.bashrc

export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"


test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Bash
export PATH=$PATH:/usr/local/opt/go/libexec/bin;

# Go
export PATH=$PATH:$HOME/dev/go/bin;
export GOPATH=$HOME/dev/go;
export GO_ENV="development";

# NodeJS
export NODE_ENV="development";
