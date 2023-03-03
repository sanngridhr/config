if status is-interactive
   # aliases
    alias fish_greeting yass
    alias cat=bat
    alias ls="exa -lFh --git"

   # abbreviations
    abbr a "abbr"
    abbr i "yay -Si"
    abbr R "yay -Rns"
    abbr S "yay -S"
    abbr v "nvim"
    abbr y "yay"
    abbr e "exit"
    abbr q "exit"
    abbr s "sudo"

   # variaples
    set -gx PAGER most
    set -gx EDITOR nvim
    fish_add_path ~/.local/bin/

   # starship
    starship init fish | source
end
