
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt beep extendedglob
# End of lines configured by zsh-newuser-install

# completion
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion::complete:*' gain-privileges 1

# plugins
source /usr/share/zsh/plugins/zsh-sudo/sudo.plugin.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-abbr/zsh-abbr.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

# aliases
alias cat=bat
alias ls="exa -lFh --git"

# export variables
export PAGER=most
export PATH=~/.emacs.d/bin/:$PATH
export BAT_THEME=OneHalfDark

# terminal art and prompt
/data/projects/sysfetch/sysfetch
eval "$(starship init zsh)"
