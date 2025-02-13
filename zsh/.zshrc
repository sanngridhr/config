# XDG fixes
export XDG_CONFIG_HOME="$HOME/.config"
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/pythonrc
export STARSHIP_CONFIG="$XDG_CONFIG_HOME"/starship/starship.toml
export TEXMFHOME="$XDG_CONFIG_HOME"/texmf
export VIMINIT='let $MYVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/vimrc" : "$XDG_CONFIG_HOME/nvim/init.vim" | so $MYVIMRC'
alias d-fi="d-fi -conf $XDG_CONFIG_HOME/d-fi/config.json"

export XDG_DATA_HOME="$HOME/.local/share"
export ANDROID_HOME="$XDG_DATA_HOME"/android
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export GOPATH="$XDG_DATA_HOME"/go
export NIMBLE_DIR="$XDG_DATA_HOME"/nimble
export OPAMROOT="$XDG_DATA_HOME"/opam
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export TERMINFO="$XDG_DATA_HOME"/terminfo
export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo
export W3M_DIR="$XDG_DATA_HOME"/w3m
alias wget="wget --hsts-file='$XDG_DATA_HOME/wget-hsts'"

export XDG_CACHE_HOME="$HOME/.cache"
export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority/texlive/texmf-var
export PKG_CACHE_PATH="$XDG_CACHE_HOME"/pkg-cache
export TEXMFVAR="$XDG_CACHE_HOME"/texlive/texmf-var

export XDG_STATE_HOME="$HOME/.local/state"

export GHCUP_USE_XDG_DIRS=1

# $PATH fixes
typeset -U path PATH
path=(~/.local/bin ~/.config/emacs/bin ~/.deno/bin $CARGO_HOME/bin $GOPATH/bin $NIMBLE_DIR/bin $path)
export PATH

# Opam setup
if [[ ! -r /home/orest/.local/share/opam/opam-init/init.zsh ]]; then
   source /home/orest/.local/share/opam/opam-init/init.zsh > /dev/null 2> /dev/null
fi

# Only in interactive mode
if [[ $- == *i* ]]; then
   # Greeting
   mycofetch -m $XDG_CONFIG_HOME/mycofetch/template.mfc

   # Shell configuration
   setopt autocd beep noextendedglob nomatch

   # History configuration
   HISTSIZE=5000
   SAVEHIST=$HISTSIZE
   HISTFILE=$ZDOTDIR/.histfile
   setopt appendhistory
   setopt INC_APPEND_HISTORY  
   setopt SHARE_HISTORY
   setopt histignorealldups

   # Bindings
   bindkey -e
   bindkey "^[[H" beginning-of-line # Home
   bindkey "^[[F" end-of-line # End
   bindkey "^[[3~" delete-char # Delete
   bindkey "^[[1;5C" forward-word # C-Right
   bindkey "^[[1;5D" backward-word # C-Left

   # Variables
   export EDITOR="emacs -nw"
   export FZF_DEFAULT_COMMAND="fd -H"
   export GPG_TTY=$(tty)
   export GUILE_AUTO_COMPILE=0
   export MICRO_TRUECOLOR=1
   export MANPAGER="bat -l man -p"
   export PAGER=less

   # Aliases
   alias cat="bat -n"
   alias cp="cp -v"
   alias grep="grep -ni --color"
   alias imv=imv-dir
   alias ls="eza -F -Ghl --git --icons --sort type"
   alias mv="mv -v"
   alias mycofetch="$HOME/.local/bin/mycofetch -m $XDG_CONFIG_HOME/mycofetch/template.mfc"
   alias nix-shell="nix-shell --run $SHELL"
   alias t503d="sudo nix-shell --run 'python3 /data/build/10moons-t503-driver/driver.py' \
                     -p python3 python3Packages.evdev python3Packages.pyusb python3Packages.pyyaml"
   alias nixos-rebuild="nixos-rebuild --log-format multiline-with-logs --use-remote-sudo"
   alias rg="rg -ip"
   alias xclass="xprop | grep WM_CLASS | awk '{ print $4 }'"
   find /opt/plan9 &> /dev/null && alias 9=/opt/plan9/bin/9

   # zcomet setup
   if [ ! -f ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh ]; then
      command git clone https://github.com/agkozak/zcomet.git ${ZDOTDIR:-${HOME}}/.zcomet/bin
   fi

   source ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh

   # Completion
   zstyle ':completion:*' menu select
   zmodload zsh/complist
   zcomet compinit
   zcomet load zsh-users/zsh-completions

   # Abbreviations
   zcomet load olets/zsh-abbr

   # Syntax highlighting
   zcomet load zdharma-continuum/fast-syntax-highlighting

   # Substring history search
   zcomet load zsh-users/zsh-history-substring-search
   bindkey '^[[A' history-substring-search-up
   bindkey '^[[B' history-substring-search-down

   # Autosuggestions
   zcomet load zsh-users/zsh-autosuggestions

   # Starship
   eval "$(starship init zsh)"
fi
